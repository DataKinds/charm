{-# LANGUAGE ViewPatterns #-}

module Stage2.TypeChecker where

import Stage1.CharmParser
import qualified Data.Map.Strict as M
import Control.Monad.State
import Control.Monad.Writer
import Data.List

--                                   stack in   , stack out
type TypeEnvironment = M.Map String ([CharmTypeTerm], [CharmTypeTerm])

pruneTypeSignatures :: [CharmTerm] -> State TypeEnvironment [CharmTerm]
pruneTypeSignatures (term:terms) = case term of
  (CharmTypeSig name types) -> do
    modify (M.insert name types)
    pruneTypeSignatures terms
  _ -> do
    pruned <- pruneTypeSignatures terms
    return (term:pruned)
pruneTypeSignatures [] = return []

typeMatch :: CharmTypeTerm -> CharmTypeTerm -> Bool
typeMatch (CharmType "Any") _ = True
typeMatch _ (CharmType "Any") = False
typeMatch a b = a == b

-- Take a stack of types and see if you can apply a function to it
unifyTypes :: [CharmTypeTerm] -- The types on the stack before this call
           -> ([CharmTypeTerm], [CharmTypeTerm]) -- Function's type
           -> Either String [CharmTypeTerm] -- Either an error message or the remaining types on the stack
unifyTypes pre sig =
  let
    matched = zipWith typeMatch pre (fst sig)
  in
    case and matched of
      True -> Right $ (drop (length sig) pre) ++ (snd sig)
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
unifyWithEnv env (Right pre) (CharmNumber _) = unifyTypes pre ([], [CharmType "Num"])
unifyWithEnv env (Right pre) (CharmString _) = unifyTypes pre ([], [CharmType "String"])
unifyWithEnv env (Right pre) (CharmList _) = unifyTypes pre ([], [CharmType "List"])
unifyWithEnv env err@(Left _) next = err

check :: TypeEnvironment -- The type signatures to check 
      -> [CharmTerm]     -- The functions themselves
      -> Either String [CharmTypeTerm] -- Either an error or success (current stack types)
check env terms = foldr (flip $ unifyWithEnv env) (Right []) terms

checkGoal :: TypeEnvironment -- The environment type signatures
          -> ([CharmTypeTerm], [CharmTypeTerm]) -- The "goal" type signature
          -> [CharmTerm] -- The functions to check
          -> Either String [CharmTypeTerm] -- Either an error or success (current stack types)
checkGoal env goal@(pre, post) terms =
  let
    unified = foldr (flip $ unifyWithEnv env) (Right pre) terms
  in
    case unified of
      err@(Left _) -> err
      (Right ((== post) -> True)) -> Right post
      (Right post') -> Left $ "Couldn't match given type signature\n    " ++ show goal ++ "\n with actual type\n    " ++ show post'


--- PRELUDE FUNCTION TYPES ---
type T = ([CharmTypeTerm], [CharmTypeTerm])

registerType :: String -> T -> TypeEnvironment -> TypeEnvironment
registerType = M.insert

defaultEnv :: TypeEnvironment
defaultEnv =
  registerType "p" ([CharmType "Any"], []) .
  registerType "pstring" ([CharmType "String"], []) .
  registerType "getline" ([], [CharmType "String"]) .
  registerType "newline" ([], []) .
  registerType "type" ([CharmType "Any"], [CharmType "Any", CharmType "String"]) $ M.empty
