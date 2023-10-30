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
  import Data.Functor.Identity
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

And finally, this is just a helper function for running the
typechecker with an empty internal state.

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
      go x@(TypeVar _) y = do
        concX <- lift $ concretize x
        case concX of
          -- An unbound type var found itself onto the stack!
          -- It already failed to match another type var by name, so let's throw
          TypeVar a -> throwError $ TypeErrorGotXWantedY x y
          -- 
            
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
      -- One of the branches was unbalanced. This is an error condition.
      go x@(TypeAlternative [] _) y@(TypeAlternative _ _) = throwError $ TypeErrorGotXWantedY x y
      go x@(TypeAlternative _ []) y@(TypeAlternative _ _) = throwError $ TypeErrorGotXWantedY x y
      go x@(TypeAlternative _ _) y@(TypeAlternative [] _) = throwError $ TypeErrorGotXWantedY x y
      go x@(TypeAlternative _ _) y@(TypeAlternative _ []) = throwError $ TypeErrorGotXWantedY x y
      -- TypeAlternatives don't unify with other types
      go x@(TypeAlternative _ _) y = throwError $ TypeErrorGotXWantedY x y
      go x y@(TypeAlternative _ _) = throwError $ TypeErrorGotXWantedY x y
\end{code}

Now we introduce the most general of the unification functions:
`applyAndUnify`. It ingests the state of the stack along with a
function's full type signature (both popped and pushed types). It then
returns the _underflow_ and the _overflow_ of the function with
respect to the stack.

The _underflow_ is the first element of the result tuple. It
represents any type that was popped while the stack was empty. In a
forward type-checking context, this represents an Underflow error. In
a type-inference context, this may be used to infer the popped
arguments of a nest or function.

The _overflow_ is the second element of the result tuple. It
represents any types left on the stack after the evaluation of the
described function. It is made as concrete as possible -- all
variables bound in the TypeContext are substituted before getting
pushed.

Note that the names of `TypeVar`s may be mangled if they overlap with
`TypeVar`s that have already been quantified on the stack. Remember
that type quantifiers only exist at the top level of a function's
signature atm. In the future, there needs to be a way to store
(un)bound type variables along with the type signatures themselves.

This function can fail and throw a `CharmTypeError` in many ways, but
the most notable is that it throws a `TypeErrorGotXWantedY` if the
stack and the popped types don't unify.
 
\begin{code}
  -- | Mangles the name of any type var on the stack if it conflicts
  -- with a newly quantified type var.
  mangleVars :: [CharmType] -> [CharmType] -> [CharmType]
  mangleVars stack pops = stack

  -- TODO: mangle the pre-evaluation stack before beginning
 
  -- | This is the most general of the unification functions
  -- | Takes the stack, a func's popped types, a func's pushed types,
  -- | and returns either a type unification error on failure,
  -- | or the state of the stack after applying the func. This state
  -- | includes both the underflowed values the func attempted to pop,
  -- | along with the values pushed back onto the stack.
  applyAndUnify
    :: [CharmType] -- Pre-evaluation stack
    -> [CharmType] -- The func's popped types, half its signature
    -> [CharmType] -- The func's pushed types, the other half of its signature
    -> ExceptT CharmTypeError TypeContext ([CharmType], [CharmType]) -- Either a unification error or the stack under/overflow
  applyAndUnify = go ([], [])
    where
      -- | A wrapper that tracks intermediate underflow / overflow state
      go :: ([CharmType], [CharmType]) -> [CharmType] -> [CharmType] -> [CharmType] -> ExceptT CharmTypeError TypeContext ([CharmType], [CharmType])
      -- First, pop all the function's popped types, checking that
      -- they unify with what's on the stack
      go (under, over) (top:stack) (pop:pops) pushes = do
        leftoverTypes <- unifyOne top pop
        go (under, over) (leftoverTypes ++ stack) pops pushes
      -- If the stack runs dry, start tracking its underflow
      go (under, over) [] (pop:pops) pushes = do
        concPop <- lift $ concretize pop
        go (concPop:under, over) [] pops pushes
      -- Once we've popped all that the function wanted to pop, push
      -- the types the function wanted to push
      go (under, over) stack [] (push:pushes) = do
        concPush <- lift $ concretize push
        go (under, concPush:over) stack [] pushes
      -- Finally, if there's no more to push or pop, we've got our
      -- final underflow and overflow states
      go underover stack [] [] = pure underover

  -- | A wrapper for applyAndUnify that discards the TypeContext at the
  -- | end of the application. This function is usually more useful, as
  -- | quantifiers cannot extend past function borders anyway.
  evalAndUnify :: [CharmType] -> [CharmType] -> [CharmType] -> Except CharmTypeError ([CharmType], [CharmType])
  evalAndUnify stack pops pushes = mapExceptT forget $ applyAndUnify stack pops pushes
    where
      forget :: TypeContext (Either CharmTypeError ([CharmType], [CharmType])) -> Identity (Either CharmTypeError ([CharmType], [CharmType]))
      forget tc = fst <$> runTypeContext tc
  
\end{code}

Unifies (or fails to unify) a single function `f` when the stack has a
known sequence of types on the top of it.

\begin{code}
  unify
    :: [CharmType]                                    -- Pre-evaluation stack types
    -> [CharmType]                                    -- f's popped types
    -> ExceptT CharmTypeError TypeContext [CharmType] -- Either a unification error or the post-evaluation stack-
  unify (x:lhs) (y:rhs) = do
    leftoverTypes <- unifyOne x y
    recurseTypes <- unify (leftoverTypes ++ lhs) rhs
    return recurseTypes
  unify [] [] = pure []
  unify lhs [] = pure lhs
  unify [] (y:_) = throwError $ TypeErrorStackUnderflow y

\end{code}

Type inference
--------------

The following section is concerned with inferring the stack effects a
set of functions would have if they were executed.

First, a function to lift single AST terms to their types.

\begin{code}
  liftAST
    :: TypeEnvironment -- The known type signatures of all functions
    -> CharmAST        -- The AST term to lift
    -> Except CharmTypeError ([CharmType], [CharmType])      -- The types popped and pushed, respectively, by the AST term
  liftAST te (ASTName n) = case M.lookup n te of
      Just (popped, pushed) -> return (popped, pushed)
      Nothing -> throwError $ TypeErrorUnknownFunction n
  liftAST te (ASTNumber _) = return ([], [TypeConcrete "Num"])
  liftAST te (ASTQuote _) = return ([], [TypeConcrete "Str"])
  liftAST te (ASTNest body) = infer te body
  liftAST te (ASTDefinition _ _) = return ([], [])
  liftAST te _ = throwError $ TypeErrorFatal (Just "liftAST got unexpected type-level term!")
\end{code}

Alongside a coroutine that lifts sequences of AST terms to their types 

\begin{code}
  infer
    :: TypeEnvironment
    -> [CharmAST]
    -> Except CharmTypeError ([CharmType], [CharmType])
  infer = go ([], [])
    where
      go :: ([CharmType], [CharmType]) -> TypeEnvironment -> [CharmAST] -> Except CharmTypeError ([CharmType], [CharmType])
      go (under, over) te (ast:asts) = do
        (pops, pushes) <- liftAST te ast
        (under', over') <- evalAndUnify over pops pushes
        -- TODO: the typevars on the stack must be mangled before
        -- combining to preserve quantifiers
        go (under' ++ under, over' ++ over) te asts
      go underover te [] = pure underover
\end{code}
