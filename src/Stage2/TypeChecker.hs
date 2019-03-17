{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Stage2.TypeChecker where

import Stage1.CharmParser
import qualified Data.Map.Strict as M
import Control.Monad.State
import Control.Monad.Writer
import Data.List
import Data.Bifunctor
import qualified Data.Set as S
import Debug.Trace
  
--              stack in   , stack out
type TypeEnvironment = M.Map String T

pruneTypeSignatures :: [CharmTerm] -> State TypeEnvironment [CharmTerm]
pruneTypeSignatures (term:terms) = case term of
  (CharmTypeSig name types) -> do
    modify (M.insert name types)
    pruneTypeSignatures terms
  _ -> do
    pruned <- pruneTypeSignatures terms
    return (term:pruned)
pruneTypeSignatures [] = return []

-- TODO: properly implement CharmTypeQ
typeMatch :: M.Map String CharmTypeTerm -- The bound type variables
          -> CharmTypeTerm -> CharmTypeTerm
          -> Bool
typeMatch bound (CharmType "Any") _ = True
typeMatch bound (CharmType a) (CharmType b) = a == b
typeMatch bound (CharmType a) (CharmTypeQ b) = a == b
typeMatch bound a@(CharmType _) (CharmTypeVar b) =
  case M.lookup b bound of
    Just b' -> a == b'
    Nothing -> False
typeMatch bound (CharmTypeQ a) (CharmTypeQ b) = a == b
typeMatch bound a@(CharmTypeQ _) (CharmTypeVar b) =
  case M.lookup b bound of
    Just b' -> a == b'
    Nothing -> False
typeMatch bound (CharmTypeVar a) (CharmTypeVar b) =
  case M.lookup a bound of
    Just a' -> case M.lookup b bound of
      Just b' -> a' == b'
      Nothing -> False
    Nothing -> False
typeMatch bound a b = typeMatch bound b a

-- Uses the types on the passed in stack to derive concrete values for type variables
getTypeVarBindings' :: [CharmTypeTerm] -- The types on the stack pre function call
                    -> T -- The function type
                    -> M.Map String CharmTypeTerm -- Recursive term
                    -> Either String (M.Map String CharmTypeTerm) -- Either an error or a map of bound variables
getTypeVarBindings' (top:pre) sig@(T ((CharmTypeVar top'):pre') post') rec =
  case M.lookup top' rec of
    (Just t) -> (if t == top then getTypeVarBindings' pre (T pre' post') rec else Left $ "Couldn't reconcile type variable\n    " ++ top' ++ "\nThis type variable was bound to the type\n    " ++ show t ++ "\nbut took on the type\n    " ++ show top)
    Nothing -> getTypeVarBindings' pre (T pre' post') (M.insert top' top rec)
getTypeVarBindings' (top:pre) sig@(T (top':pre') post') rec = getTypeVarBindings' pre (T pre' post') rec
getTypeVarBindings' [] sig@(T [] post') rec = Right rec
getTypeVarBindings' pre sig@(T [] post') rec = Right rec
getTypeVarBindings' [] sig rec = Left $ "Exhausted the stack while binding type variables for type\n    " ++ show sig

getTypeVarBindings pre sig = getTypeVarBindings' pre sig M.empty

bindTypeVars :: [CharmTypeTerm] -> M.Map String CharmTypeTerm -> Either String [CharmTypeTerm]
bindTypeVars ts vars = sequence $
  (\case
      CharmTypeVar t ->
        case M.lookup t vars of
          Just binding -> Right binding
          Nothing -> Left $ "Unbound type variable\n    " ++ t ++ "\nWhile binding type signature\n    " ++ show ts
      t -> Right t) <$> ts

-- TODO: fix literally everything that uses this
isTypeVar (CharmTypeVar _) = True
isTypeVar _ = False
typeVar (CharmTypeVar s) = s
typeVar _ = error "miscalled `typeVar`"

-- Make sure that type variables are all introduced
-- on the left side before being used on the right side
checkTypeVarExistence :: T -> Either String ()
checkTypeVarExistence sig@(T pre post) =
  let
    preVars = S.fromList $ filter isTypeVar pre
    postVars = S.fromList $ filter isTypeVar post

    nonexistentVars = postVars S.\\ preVars
  in
    case null nonexistentVars of
      True -> Right ()
      False -> Left $ "Nonexistent type variables " ++ show nonexistentVars ++ " were introduced."

-- Take a stack of types and see if you can apply a function to it
unifyTypes :: [CharmTypeTerm] -- The types on the stack before this call
           -> T -- Function's type
           -> Either String [CharmTypeTerm] -- Either an error message or the remaining types on the stack
unifyTypes pre sig@(T pre' post') = do
  checkTypeVarExistence sig
  vars <- getTypeVarBindings pre sig
  boundPre <- bindTypeVars pre' vars
  boundPost <- bindTypeVars post' vars
  let matched = zipWith (typeMatch vars) pre boundPre
  case and matched of
    True -> Right $ (boundPost) ++ (drop (length pre') pre)
    False -> Left $ "Couldn't unify given type\n    " ++ show sig ++ "\nand expected type\n    " ++ show pre

-- Same as unifyTypes, but resolves type signatures through the TypeEnvironment
-- For use with `foldr` in `check` and `checkGoal`
unifyWithEnv :: TypeEnvironment
             -> Either String [CharmTypeTerm] -- An error, or the types on the stack
             -> CharmTerm -- The term to resolve or check (this includes definitions)
             -> Either String [CharmTypeTerm] -- Either an error message or the remaining types on the stack
unifyWithEnv env (Right pre) (CharmIdent termname) =
  case M.lookup termname env of
    Nothing -> Left $ "Couldn't find type signature for function\n    " ++ show termname
    Just t -> unifyTypes pre t
unifyWithEnv env (Right pre) (CharmDef fname def) =
  case M.lookup fname env of
    Nothing -> Left $ "Couldn't find type signature for function definition\n    " ++ show fname
    Just t -> checkGoal env t def
unifyWithEnv env (Right pre) (CharmNumber _) = unifyTypes pre (T [] [CharmType "Num"])
unifyWithEnv env (Right pre) (CharmString _) = unifyTypes pre (T [] [CharmType "String"])
unifyWithEnv env (Right pre) (CharmList _) = unifyTypes pre (T [] [CharmType "List"])
unifyWithEnv env err@(Left _) next = err

check :: TypeEnvironment -- The type signatures to check 
      -> [CharmTerm]     -- The functions themselves
      -> Either String [CharmTypeTerm] -- Either an error or success (current stack types)
check env terms = foldr (flip $ unifyWithEnv env) (Right []) terms

checkGoal :: TypeEnvironment -- The environment type signatures
          -> T -- The "goal" type signature
          -> [CharmTerm] -- The functions to check
          -> Either String [CharmTypeTerm] -- Either an error or success (current stack types)
checkGoal env goal@(T pre post) terms =
  let
    unified = foldr (flip $ unifyWithEnv env) (Right pre) terms
  in
    case trace ("unified is: " ++ show unified) unified of
      err@(Left _) -> err
      (Right ((== post) -> True)) -> Right post
      (Right post') -> Left $ "Couldn't match given type signature\n    " ++ show goal ++ "\n with actual type\n    " ++ show (T pre post')


--- PRELUDE FUNCTION TYPES ---
registerType :: String -> T -> TypeEnvironment -> TypeEnvironment
registerType = M.insert

defaultEnv :: TypeEnvironment
defaultEnv =
  registerType "p" (T [CharmType "Any"] []) .
  registerType "pstring" (T [CharmType "String"] []) .
  registerType "getline" (T [] [CharmType "String"]) .
  registerType "newline" (T [] []) .
  registerType "type" (T [CharmType "Any"] [CharmType "Any", CharmType "String"]) $ M.empty
