{-# LANGUAGE ViewPatterns #-}

module Stage2.TypeChecker where

import Stage1.CharmParser
import qualified Data.Map.Strict as M
import Control.Monad.State
import Control.Monad.Writer
import Data.List

--                                   stack in   , stack out
type TypeEnvironment = M.Map String ([CharmTerm], [CharmTerm])

pruneTypeSignatures :: [CharmTerm] -> State TypeEnvironment [CharmTerm]
pruneTypeSignatures (term:terms) = case term of
  (CharmTypeSig name types) -> do
    modify (M.insert name types)
    pruneTypeSignatures terms
  _ -> do
    pruned <- pruneTypeSignatures terms
    return (term:pruned)
pruneTypeSignatures [] = return []

typeMatch :: CharmTerm -> CharmTerm -> Bool
typeMatch (CharmIdent "any") _ = True
typeMatch _ (CharmIdent "any") = False
typeMatch a b = a == b

unifyTypes :: [CharmTerm] -- The types on the stack before this call
            -> ([CharmTerm], [CharmTerm]) -- Function's type
            -> Either String [CharmTerm] -- Either an error message or the remaining types on the stack
unifyTypes pre sig =
  let
    paddedPre = if (length pre < length (fst sig)) then replicate (length (fst sig) - length pre) (CharmIdent "int") ++ pre else pre
    matched = zipWith typeMatch paddedPre (fst sig)
  in
    case and matched of
      True -> Right $ (drop (length sig) pre) ++ (snd sig)
      False -> Left $ "Couldn't unify given type\n    " ++ show sig ++ "\nand expected type\n    " ++ show pre

check :: TypeEnvironment -- The type signatures to check 
      -> [CharmTerm]     -- The functions themselves
      -> Either String [CharmTerm] -- Either an error or success (current stack types)
check env terms =
  let
    unifyWithEnv (Right pre) (CharmIdent termname) =
      case M.lookup termname env of
        Nothing -> Left $ "Couldn't find type signature for function\n    " ++ show termname
        Just t -> unifyTypes pre t
    unifyWithEnv err@(Left _) next = err
  in
    foldr (flip unifyWithEnv) (Right []) terms

checkGoal :: TypeEnvironment -- The environment type signatures
          -> ([CharmTerm], [CharmTerm]) -- The "goal" type signature
          -> [CharmTerm] -- The functions to check
          -> Either String [CharmTerm] -- Either an error or success (current stack types)
checkGoal env goal@(pre, post) terms =
  let
    unifyWithEnv (Right pre') (CharmIdent termname) =
      case M.lookup termname env of
        Nothing -> Left $ "Couldn't find type signature for function\n    " ++ show termname
        Just t -> unifyTypes pre' t
    unifyWithEnv err@(Left _) next = err
    unified = foldr (flip unifyWithEnv) (Right pre) terms
  in
    case unified of
      err@(Left _) -> err
      (Right ((== post) -> True)) -> Right post
      (Right post') -> Left $ "Couldn't match given type signature\n    " ++ show goal ++ "\n with actual type\n    " ++ show post'


--- PRELUDE FUNCTION TYPES ---
type T = ([CharmTerm], [CharmTerm])

ch_t_p :: T
ch_t_p = ([CharmIdent "any"], [])

ch_t_pstring :: T
ch_t_pstring = ([CharmIdent "string"], [])

ch_t_newline :: T
ch_t_newline = ([], [])

ch_t_getline :: T
ch_t_getline = ([], [CharmIdent "string"])

ch_t_type :: T
ch_t_type = ([CharmIdent "any"], [CharmIdent "any", CharmIdent "string"])

ch_t_def :: T
ch_t_def = ([CharmIdent "string", CharmIdent "list"], [])
