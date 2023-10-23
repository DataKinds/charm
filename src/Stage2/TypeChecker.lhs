This is the typechecker.
========================

Its main purpose is twofold:

1. The typechecker unifies types. This means that it may ingest type signatures
from functions (that is, what the function pushes and pops) along with the state
of the stack when the function is to be invoked. The function's signature then
either successfully unifies with the types on the stack, or it fails and emits a
type error. The interface to the type unifier is the `unify` function.

2. The typechecker infers types. This means that it may ingest a sequence of
type signatures, then emit the final state of the stack after evaluating
functions in order. This may produce unbound type variables and thus can be used
to infer the type of an unknown sequence of functions. The inferface to the type 
inferrer is the `infer` function.

Let's start by establishing the imports we need.

\begin{code}
  {-# LANGUAGE ViewPatterns #-}

  module Stage2.TypeChecker where

  import Control.Monad
  import Control.Monad.Except
  import Control.Monad.Trans.Class
  import Control.Monad.Trans.Except
  import Control.Monad.Trans.State
  import Control.Monad.Trans.Writer.CPS
  import Data.Bifunctor
  import Data.Function ()
  import Data.List
  import Debug.Trace
  import qualified Data.Char as C
  import qualified Data.Map.Strict as M
  import qualified Data.Set as S
  import Stage1.CharmParser
\end{code}

Let's establish some types to do typechecking with. The `CharmType` data type is
the space of representable type terms within Charm. Each term is written next to
the in-language syntax that represents it. The span of representable types are
as follows:

* The `TypeConcrete` is a fully instantiated type, like `String` or `Num`. 

* The `TypeNest` is the type of anonymous functions which push a set of types
and pop a set of types from the stack.

* The `TypeVar` is a type variable existentially quantified at the top level of
the type signature. It can stand in for one singular type term. Note that we
currently restrict quantifiers to appearing on the top level, so there is no
equivalent to Haskell's `RankNTypes`.

* The `TypeAlternative` is a type which can represent one of two sequences of
type terms. There are two restrictions to forming a valid `TypeAlternative`. The
branches of the alternative must be the same length. Additionally, the two
branches of the type alternative must not share a nonempty prefix, else it will
be factored out of the alternative. 

* The `TypeOptional` indicates a type which might be `nil`. Note that `nil` has
type `Any?` and therefore unifies with every `TypeOptional` regardless of its
content.

* The `TypeAny` is a top type. All singular type terms unify against it and it
may be used as a sort of "hatch" out of the type system.

\begin{code}
  data CharmType =
    TypeNest [CharmType] [CharmType]          -- [A B C -> X Y Z]
    | TypeVar String                          -- a
    | TypeConcrete String                     -- Num, String, etc
    | TypeVariadic CharmType                  -- A...
    | TypeAlternative [CharmType] [CharmType] -- (A B C | X Y Z)
    | TypeOptional CharmType                  -- A?
    | TypeAny                                 -- Any
    deriving (Eq)

  instance Show CharmType where
    show (TypeNest as bs) = "[" ++ s as ++ " -> " ++ s bs ++ "]"
      where
        s :: [CharmType] -> String
        s ts = unwords $ show <$> ts
    show (TypeVar v) = v
    show (TypeConcrete c) = c
    show (TypeVariadic t) = show t ++ "..."
    show (TypeAlternative as bs) = "(" ++ s as ++ " | " ++ s bs ++ ")"
      where
        s :: [CharmType] -> String
        s ts = unwords $ show <$> ts
    show (TypeOptional t) = show t ++ "?"
    show (TypeAny) = "Any"

\end{code}


These are helper types that are mostly internal to the typechecker. 

What Charm functions have what type? This is returned by `extractTypesFromAST`
after walking the AST.

> type TypeEnvironment = M.Map String ([CharmType], [CharmType]) 

What type variables have a concrete binding? 
Right now, this is the entirety of the internal state of the typechecker.

> type TypeVarBindings = M.Map String CharmType

Functions which return a (TypeContext :: * -> *) need access to the internal
state of the typechecker.

> type TypeContext = State TypeVarBindings 

And finally, this is just a helper function for running the typechecker and
discarding its internal state.

> runTypeContext :: StateT (M.Map k a1) m a2 -> m (a2, M.Map k a1)
> runTypeContext tc = runStateT tc M.empty

The type system must be able to return expressive errors. These are the current
error classes we handle:

> data CharmTypeError =

The type system expected one thing here but saw another!

>  TypeErrorGotXWantedY CharmType CharmType 

The stack ran empty when it shouldn't have! Here's what was popped.

>  | TypeErrorStackUnderflow CharmType

A function pushed more than it said it would in its type signature. Here's what
it pushed.

>  | TypeErrorPushedTooMuch [CharmType]

A type variable did not appear on the LHS of a function before appearing on the
RHS. 

>  | TypeErrorUnboundVar String
>  | TypeErrorUnimplemented CharmType

The user tried to call a function which the typechecker doesn't know about!

>  | TypeErrorUnknownFunction String
>  | TypeErrorFatal (Maybe String)
>  deriving (Show)

Converting ASTs to `CharmType`s
-------------------------------

This section is full of functions which extract and strip type signatures from
program ASTs.

Parse a single AST node: take a single AST term and transform it into its
CharmType representation.

\begin{code}
  typeFromAST :: CharmAST -> Either String CharmType
  typeFromAST = go
    where
      go :: CharmAST -> Either String CharmType
      go (ASTName ident@(and . fmap C.isLower -> True)) = Right $ TypeVar ident
      go (ASTName ident) = Right $ TypeConcrete ident
      go (ASTTypeVariadic ast) = do
        typeAst <- go ast
        return $ TypeVariadic typeAst
      go (ASTTypeAlternative l r) = do
        typeL <- sequence $ map go l
        typeR <- sequence $ map go r
        -- Disallow common prefixes in alternative statements
        let commonPrefixLen = length . takeWhile (id) $ zipWith (==) typeL typeR
        when (commonPrefixLen > 0) (throwError $ "alternative has common prefix " ++ show (take commonPrefixLen typeL))
        -- TODO: disallow different-length branches
        return $ TypeAlternative typeL typeR
      go (ASTTypeNest pre post) = do
        typePre <- sequence $ go <$> pre
        typePost <- sequence $ go <$> post
        return $ TypeNest typePre typePost
      go (ASTNumber _) = Left "unexpected numeral in type!"
      go (ASTQuote _) = Left "unexpected string in type!"
      go (ASTNest _) = Left "bare nest in type! expected a '->'!"
      go (ASTDefinition _ _) = Left "malformed definition in type!"
\end{code}

Parse many AST nodes: takes a whole Charm program and reads the types from it,
saving them in a TypeEnvironment. Can output errors if a type is malformed
entirely. The returned result is the AST stripped of type signatures.

\begin{code}
  extractTypesFromAST :: [CharmAST] -> WriterT [String] (State TypeEnvironment) [CharmAST]
  extractTypesFromAST = \case
    [] -> return []
    (ASTTypeAssignment name pretypes posttypes):rest -> do
      let typeSigWithError = (typeFromAST $ ASTTypeNest pretypes posttypes) :: Either String CharmType
      case typeSigWithError of
        Left err -> tell [err]
        Right (TypeNest lhs rhs)-> lift $ modify (M.insert name (lhs, rhs))
      extractTypesFromAST rest
    term:rest -> do
      pruned <- extractTypesFromAST rest
      return (term:pruned)
\end{code}

Type unification 
----------------

The next section is concerned with performing type unification and returning
sensible errors on unification failure.

Given the current typechecker state, convert all `TypeVar`s to their concrete
representations.

\begin{code}
  concretize :: CharmType -> TypeContext CharmType
  concretize (TypeNest pre post) = do
    concPre <- sequence $ concretize <$> pre
    concPost <- sequence $ concretize <$> post
    return $ TypeNest concPre concPost
  concretize (TypeAlternative l r) = do
    concL <- sequence $ concretize <$> l
    concR <- sequence $ concretize <$> r
    return $ TypeAlternative concL concR
  concretize (TypeVariadic t) = do
    concT <- concretize t
    return $ TypeVariadic concT
  concretize (TypeVar v) = get >>= (\ctx -> return $ case M.lookup v ctx of
    Just concV -> concV
    Nothing -> TypeVar v)
  concretize t = return t
\end{code}


Now we're dealing with the big guns! The `unifyOne` function attempts to unify
one single type term with another single type term. On failure it throws a type
error, on success it gives back what's left on the stack (which should always be
empty, now that I'm thinking of it? unless we allow `TypeVariadic` to be
partially unified.)

A note about quantifiers: we're only allowing rank 1 types for now. This
means that quantifiers are always at the very start of a function's
signature. if there's an:

    f : a b [b -> c] -> c

then there's an implicit forall quantifying all variables at the start of the
top level function's signature, here:

    f : (forall a b c.) a b [b -> c] -> c

\begin{code}
  unifyOne
    :: CharmType                         -- LHS type term (ie Pre-evaluation stack type)
    -> CharmType                         -- RHS type term (ie Function's popped type)
    -> ExceptT CharmTypeError TypeContext [CharmType] -- Either a unification error or what's left of the LHS after unification
  unifyOne lhs rhs = do
    guard lhs rhs
    go lhs rhs
    where
      guard :: CharmType -> CharmType -> ExceptT CharmTypeError TypeContext [CharmType]
      guard lhs rhs = do
        concLhs <- lift $ concretize lhs
        concRhs <- lift $ concretize rhs
        -- There shouldn't ever be unbound variables on the LHS
        -- when (concLhs /= lhs) $ throwError (TypeErrorFatal $ Just "Unbound var on LHS!")
        pure []
\end{code}

`go` forms the bulk of the type unification code. It encodes the rules for how
different type terms unify. 

It can be a useful mnemonic to think of the LHS "consuming" the RHS here. This
type unification is used when functions are popping existing types from off the
stack, so whatever's fed in on the RHS generally expects to be able to "pop" the
LHS.

>     go :: CharmType -> CharmType -> ExceptT CharmTypeError TypeContext [CharmType]

To unify a `TypeVar`, it's a matter of seeing if it's already bound. Bound type
vars get their bindings unified, unbound type vars get bound. If a `TypeVar`
shows up on the LHS here, that's because we're in a type inference context and
we should implement the same binding behavior.

`TypeVar`s may be bound to any `CharmType`, so this defintion must be written
first to avoid getting caught in any other type's special casing. The simplest
case is binding to a `TypeConcrete`, in which case the `TypeVar` represents a
single concrete type. However, `TypeVar`s may be bound to `TypeOptional`s,
`TypeAlternative`s, `TypeVariadic`s, and even other `TypeVar`s. The lack of
currying additionally lets us bind to `TypeNest`s unambiguously as a single type
term. 

\begin{code}
      go x@(TypeVar a) y@(TypeVar b) = case a == b of
        True -> return []
        False -> do
          concX <- lift $ concretize x
          concY <- lift $ concretize y
          -- Neither type var has been bound. 
          -- We're in an inference context, let's set the type vars equal
          when ((concX == x) && (concY == y)) (lift $ modify (M.insert b x))
          -- Now we induct to see if the type var's bound values are equal!
          go concX concY
      go x y@(TypeVar _) = do
        concY <- lift $ concretize y
        case concY of
          -- The type var hasn't been bound and needs to be!
          TypeVar v -> do
            lift $ modify (M.insert v x) 
            go x x
          -- The type var's already been bound!
          t -> go x t
      go x@(TypeVar _) y = go y x
\end{code}

`TypeAny` is unified first after `TypeVar`s so that it may force-unify with any
other type.

\begin{code}
      go TypeAny _ = pure []
      go _ TypeAny = pure []
\end{code}

`TypeOptional`s only unify with other `TypeOptional`s. There's one additional
condition under which a `TypeOptional` can unify: `A?` on the RHS can consume
`A` on the stack.

\begin{code}
      go (TypeOptional x) (TypeOptional y) = go x y
      go x@(TypeOptional _) y = throwError $ TypeErrorGotXWantedY x y
      go x y@(TypeOptional b) = go x b
\end{code}

The rules for unifying `TypeConcrete`s are simple. If they are the same type,
they unify. Nothing else may unify with a `TypeConcrete`.

\begin{code}
      go x@(TypeConcrete s) y@(TypeConcrete s') = case s == s' of
        True -> return []
        False -> throwError $ TypeErrorGotXWantedY x y
      go x y@(TypeConcrete s) = throwError $ TypeErrorGotXWantedY x y
      go x@(TypeConcrete s) y = throwError $ TypeErrorGotXWantedY x y
\end{code}

`TypeNest`s unify when they both push and pop the same things. This is
equivalent to unifying only when their pushed types unify and when their popped
types unify. Nothing else may unify with a `TypeNest`.

\begin{code}
      -- go x@(TypeNest a b) y@(TypeNest _ _) = do
      --   concY@(TypeNest c d) <- lift $ concretize y
      --   if x == concY then return [] else throwError $ TypeErrorGotXWantedY x y
      -- -- go x@(TypeNest _ _) y = throwError $ TypeErrorGotXWantedY x y
      -- go x y@(TypeNest _ _) = throwError $ TypeErrorGotXWantedY x y
      go x@(TypeNest _ _) y = throwError $ TypeErrorGotXWantedY x y
      go x y@(TypeNest _ _) = throwError $ TypeErrorGotXWantedY x y
\end{code}

`TypeVariadic`s only unify with other `TypeVaridic`s. The condition is not
checked here, but you may not use a `TypeVariadic` if it is followed by the same
type on the stack. This ensures that a function which ingests a `TypeVariadic`
can always tell when it ends.

\begin{code}
      go (TypeVariadic x) (TypeVariadic y) = go x y
      go x@(TypeVariadic _) y = throwError $ TypeErrorGotXWantedY x y
      go x y@(TypeVariadic _) = throwError $ TypeErrorGotXWantedY x y
\end{code}

`TypeAlternative`s unify when both of their respective branches unify. Note that a
few useful identities hold here:

* (A B C | D E F) ~ (D E F | A B C)
  `TypeAlternative`s are reversable.

* (A B C | D E F) ~ (A | D) (B | E) (C | F)
  `TypeAlternative`s are separable.

\begin{code}
      -- All the branches are populated, so let's unify and induct.
      go x@(TypeAlternative (a:as) (b:bs)) y@(TypeAlternative (c:cs) (d:ds)) = do
        -- We implicitly separate the `TypeAlternative` and try both orders
        aUnified <- go a c `catchError` \err -> go a d
        bUnified <- go b d `catchError` \err -> go b c
        go (TypeAlternative as bs) (TypeAlternative cs ds)
      -- All the branches were the same length and could be fully consumed!
      go (TypeAlternative [] []) (TypeAlternative [] []) = pure []
      -- The RHS ran out in a balanced way after popping the LHS
      go x@(TypeAlternative as bs) (TypeAlternative [] []) = pure [x]
      -- One of the branches was unbalanced. T  his is an error condition.
      go x@(TypeAlternative [] _) y@(TypeAlternative _ _) = throwError $ TypeErrorGotXWantedY x y
      go x@(TypeAlternative _ []) y@(TypeAlternative _ _) = throwError $ TypeErrorGotXWantedY x y
      go x@(TypeAlternative _ _) y@(TypeAlternative [] _) = throwError $ TypeErrorGotXWantedY x y
      go x@(TypeAlternative _ _) y@(TypeAlternative _ []) = throwError $ TypeErrorGotXWantedY x y
      -- TypeAlternatives don't unify with other types
      go x@(TypeAlternative _ _) y = throwError $ TypeErrorGotXWantedY x y
      go x y@(TypeAlternative _ _) = throwError $ TypeErrorGotXWantedY x y
\end{code}
    
    
Unifies (or fails to unify) a single function `f` in a known stack type context

\begin{code}
  unify
    :: [CharmType]                       -- Pre-evaluation stack types
    -> [CharmType]                       -- f's popped types 
    -> ExceptT CharmTypeError TypeContext [CharmType] -- Either a unification error or the post-evaluation stack
  unify (x:lhs) (y:rhs) = do
    leftoverTypes <- unifyOne x y
    recurseTypes <- unify lhs rhs
    return $ leftoverTypes ++ recurseTypes
  unify [] [] = pure []
  unify lhs [] = pure lhs
  unify [] (y:_) = throwError $ TypeErrorStackUnderflow y
\end{code}
  

-- TODO: properly implement CharmTypeQ
-- typeMatch :: M.Map String CharmTypeTerm -- The bound type variables
--           -> CharmTypeTerm -> CharmTypeTerm
--           -> Bool
-- typeMatch bound (CharmType "Any") _ = True
-- typeMatch bound (CharmType a) (CharmType b) = a == b
-- typeMatch bound (CharmType a) (CharmTypeQ b) = a == b
-- typeMatch bound a@(CharmType _) (CharmTypeVar b) =
--   case M.lookup b bound of
--     Just b' -> a == b'
--     Nothing -> False
-- typeMatch bound (CharmTypeQ a) (CharmTypeQ b) = a == b
-- typeMatch bound a@(CharmTypeQ _) (CharmTypeVar b) =
--   case M.lookup b bound of
--     Just b' -> a == b'
--     Nothing -> False
-- typeMatch bound (CharmTypeVar a) (CharmTypeVar b) =
--   case M.lookup a bound of
--     Just a' -> case M.lookup b bound of
--       Just b' -> a' == b'
--       Nothing -> False
--     Nothing -> False
-- typeMatch bound a b = typeMatch bound b a

-- -- Uses the types on the passed in stack to derive concrete values for type variables
-- getTypeVarBindings' :: [CharmTypeTerm] -- The types on the stack pre function call
--                     -> T -- The function type
--                     -> M.Map String CharmTypeTerm -- Recursive term
--                     -> Either String (M.Map String CharmTypeTerm) -- Either an error or a map of bound variables
-- getTypeVarBindings' (top:pre) sig@(T ((CharmTypeVar top'):pre') post') rec =
--   case M.lookup top' rec of
--     (Just t) -> (if t == top then getTypeVarBindings' pre (T pre' post') rec else Left $ "Couldn't reconcile type variable\n    " ++ top' ++ "\nThis type variable was bound to the type\n    " ++ show t ++ "\nbut took on the type\n    " ++ show top)
--     Nothing -> getTypeVarBindings' pre (T pre' post') (M.insert top' top rec)
-- getTypeVarBindings' (top:pre) sig@(T (top':pre') post') rec = getTypeVarBindings' pre (T pre' post') rec
-- getTypeVarBindings' [] sig@(T [] post') rec = Right rec
-- getTypeVarBindings' pre sig@(T [] post') rec = Right rec
-- getTypeVarBindings' [] sig rec = Left $ "Exhausted the stack while binding type variables for type\n    " ++ show sig

-- getTypeVarBindings pre sig = getTypeVarBindings' pre sig M.empty

-- bindTypeVars :: [CharmTypeTerm] -> M.Map String CharmTypeTerm -> Either String [CharmTypeTerm]
-- bindTypeVars ts vars = sequence $
--   (\case
--       CharmTypeVar t ->
--         case M.lookup t vars of
--           Just binding -> Right binding
--           Nothing -> Left $ "Unbound type variable\n    " ++ t ++ "\nWhile binding type signature\n    " ++ show ts
--       t -> Right t) <$> ts

-- -- TODO: fix literally everything that uses this
-- isTypeVar (CharmTypeVar _) = True
-- isTypeVar _ = False
-- typeVar (CharmTypeVar s) = s
-- typeVar _ = error "miscalled `typeVar`"

-- -- Make sure that type variables are all introduced
-- -- on the left side before being used on the right side
-- checkTypeVarExistence :: T -> Either String ()
-- checkTypeVarExistence sig@(T pre post) =
--   let
--     preVars = trace ("sig: " ++ show sig) $ S.fromList $ filter isTypeVar pre
--     postVars = S.fromList $ filter isTypeVar post

--     nonexistentVars = postVars S.\\ preVars
--   in
--     case null nonexistentVars of
--       True -> Right ()
--       False -> Left $ "Nonexistent type variables " ++ show nonexistentVars ++ " were introduced."

-- -- Take a stack of types and see if you can apply a function to it
-- unifyTypes :: [CharmTypeTerm] -- The types on the stack before this call
--            -> T -- Function's type
--            -> Either String [CharmTypeTerm] -- Either an error message or the remaining types on the stack
-- unifyTypes pre sig@(T pre' post') = do
--   checkTypeVarExistence sig
--   vars <- getTypeVarBindings pre sig
--   boundPre <- bindTypeVars pre' vars
--   boundPost <- bindTypeVars post' vars
--   let matched = zipWith (typeMatch vars) pre boundPre
--   case and matched of
--     True -> Right $ (boundPost) ++ (drop (length pre') pre)
--     False -> Left $ "Couldn't unify given type\n    " ++ show sig ++ "\nand expected type\n    " ++ show pre

-- -- Same as unifyTypes, but resolves type signatures through the TypeEnvironment
-- -- For use with `foldr` in `check` and `checkGoal`
-- unifyWithEnv :: TypeEnvironment
--              -> Either String [CharmTypeTerm] -- An error, or the types on the stack
--              -> CharmTerm -- The term to resolve or check (this includes definitions)
--              -> Either String [CharmTypeTerm] -- Either an error message or the remaining types on the stack
-- unifyWithEnv env (Right pre) (CharmIdent termname) =
--   case M.lookup termname env of
--     Nothing -> Left $ "Couldn't find type signature for function\n    " ++ show termname
--     Just t -> unifyTypes pre t
-- unifyWithEnv env (Right pre) (CharmDef fname def) =
--   case M.lookup fname env of
--     Nothing -> Left $ "Couldn't find type signature for function definition\n    " ++ show fname
--     Just t -> checkGoal env t def
-- unifyWithEnv env (Right pre) (CharmNumber _) = unifyTypes pre (T [] [CharmType "Num"])
-- unifyWithEnv env (Right pre) (CharmString _) = unifyTypes pre (T [] [CharmType "String"])
-- unifyWithEnv env (Right pre) (CharmList _) = unifyTypes pre (T [] [CharmType "List"])
-- unifyWithEnv env err@(Left _) next = err

-- check :: TypeEnvironment -- The type signatures to check 
--       -> [CharmTerm]     -- The functions themselves
--       -> Either String [CharmTypeTerm] -- Either an error or success (current stack types)
-- check env terms = foldr (flip $ unifyWithEnv env) (Right []) terms

-- checkGoal :: TypeEnvironment -- The environment type signatures
--           -> T -- The "goal" type signature
--           -> [CharmTerm] -- The functions to check
--           -> Either String [CharmTypeTerm] -- Either an error or success (current stack types)
-- checkGoal env goal@(T pre post) terms =
--   let
--     unified = foldr (flip $ unifyWithEnv env) (Right pre) terms
--   in do
--     checkTypeVarExistence goal
--     case unified of
--       err@(Left _) -> err
--       (Right ((== post) -> True)) -> Right post
--       (Right post') -> Left $ "Couldn't match given type signature\n    " ++ show goal ++ "\n with actual type\n    " ++ show (T pre post')


-- --- PRELUDE FUNCTION TYPES ---
-- registerType :: String -> T -> TypeEnvironment -> TypeEnvironment
-- registerType = M.insert

-- defaultEnv :: TypeEnvironment
-- defaultEnv =
--   registerType "p" (T [CharmType "Any"] []) .
--   registerType "pstring" (T [CharmType "String"] []) .
--   registerType "getline" (T [] [CharmType "String"]) .
--   registerType "newline" (T [] []) .
--   registerType "type" (T [CharmType "Any"] [CharmType "Any", CharmType "String"]) $ M.empty
