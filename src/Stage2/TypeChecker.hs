{-# LANGUAGE ViewPatterns #-}

module Stage2.TypeChecker where

import Control.Monad.Trans.State
import Control.Monad.Trans.Writer.CPS
import Control.Monad.Trans.Class
import Control.Monad.Except
import Data.Bifunctor
import qualified Data.Char as C
import Data.List
import Data.Function
import Control.Monad
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Debug.Trace
import Stage1.CharmParser


-- Let's establish some types to do our own typechecking
data CharmType =
  TypeNest [CharmType] [CharmType]          -- A B C -> X Y Z
  | TypeVar String                          -- a
  | TypeConcrete String                     -- Num
  | TypeVariadic CharmType                  -- A...
  | TypeAlternative [CharmType] [CharmType] -- (A B C | X Y Z)
  deriving (Show, Eq)
  
type TypeEnvironment = M.Map String CharmType -- What Charm functions have what type?
type TypeVarBindings = M.Map String CharmType -- What type variables have a concrete binding? 

data CharmTypeError =
  TypeErrorGotXWantedY CharmType CharmType
  | TypeErrorUnboundVar String
  | TypeErrorFatal
  deriving (Show)

-- Takes a single AST term and transforms it into its CharmType representation
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

-- Takes a whole Charm program and strips the types from it, saving them in a TypeEnvironment
-- Can output errors if a type is malformed entirely
extractTypesFromAST :: [CharmAST] -> StateT TypeEnvironment (Writer [String]) [CharmAST]
extractTypesFromAST = \case
  [] -> return []
  (ASTTypeAssignment name pretypes posttypes):rest -> do
    let typeSigWithError = (typeFromAST $ ASTTypeNest pretypes posttypes) :: Either String CharmType
    case typeSigWithError of
      Left err -> lift $ tell [err]
      Right typeSig -> modify (M.insert name typeSig)
    extractTypesFromAST rest
  term:rest -> do
    pruned <- extractTypesFromAST rest
    return (term:pruned)

-- Convert all TypeVars to their concrete representations if possible
concretize :: TypeVarBindings -> CharmType -> CharmType
-- TODO: we could totally introduce  nested typevar quantifiers
-- but for now something like `f : [b -> c] -> c` accepts any concrete unary function (like Num -> Num)
-- as opposed to only functions with typevars like `g : x -> y`
concretize ctx (TypeNest pre post) = (TypeNest `on` fmap (concretize ctx)) pre post
concretize ctx (TypeAlternative l r) = (TypeAlternative `on` fmap (concretize ctx)) l r
concretize ctx (TypeVariadic t) = TypeVariadic . concretize ctx $ t
concretize ctx (TypeVar v) = case M.lookup v ctx of
  Just concV -> concV
  Nothing -> TypeVar v
concretize ctx t = t

unifyOne
  :: TypeVarBindings                   -- What type vars are currently bound?
  -> CharmType                         -- Pre-evaluation stack type
  -> CharmType                         -- Function's popped type
  -> Either CharmTypeError [CharmType] -- Either a unification error or what's left of the pre-evaluation stack
unifyOne = go
  where
    go :: TypeVarBindings -> CharmType -> CharmType -> Either CharmTypeError [CharmType]
    go _ x@(TypeConcrete s) y@(TypeConcrete s')     = case s == s' of
      True -> Right []
      False -> Left $ TypeErrorGotXWantedY x y
    go ctx x@(TypeConcrete s) y@(TypeVar v)      = case concretize ctx y of
      TypeConcrete concV -> go ctx x (TypeConcrete concV)
      -- TODO: this should create the binding for the type var!
      TypeVar v -> Left $ TypeErrorUnboundVar v
    go ctx x@(TypeAlternative _ _) y@(TypeAlternative _ _)
      | concX == concY = Right []
      -- TODO: partial unification?
      | otherwise = Left $ TypeErrorGotXWantedY x y
      where
        concX = concretize ctx x
        concY = concretize ctx y
    go ctx x@(TypeAlternative _ _) y = Left $ TypeErrorGotXWantedY x y
    go ctx x@(TypeNest _ _) y@(TypeNest _ _)
      -- TODO: concretizing both sides here might not be right
      | concX == concY = Right []
      | otherwise = Left $ TypeErrorGotXWantedY x y
      where
        concX = concretize ctx x
        concY = concretize ctx y
    go ctx x@(TypeNest _ _) y = Left $ TypeErrorGotXWantedY x y

    -- Invalid types to be on the stack, something went really wrong
    go _ (TypeVar _) _                         = Left TypeErrorFatal
    
-- Unifies (or fails to unify) a single function `f` in a known type context
unify
  :: [CharmType]                       -- Pre-evaluation stack types
  -> [CharmType]                       -- f's popped types 
  -> [CharmType]                       -- f's pushed types
  -> Either CharmTypeError [CharmType] -- Either a unification error or the post-evaluation stack
unify ctx pre post = undefined
  

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
