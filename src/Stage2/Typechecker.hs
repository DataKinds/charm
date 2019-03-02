module Stage2.Typechecker where

import Stage1.CharmParser
import qualified Data.Map.Strict as M
import Control.Monad.State

type TypeEnvironment = M.Map String [CharmTerm]

pruneTypeSignatures :: [CharmTerm] -> State TypeEnvironment [CharmTerm]
pruneTypeSignatures (term:terms) = case term of
  (CharmTypeSig name types) -> do
    env <- get
    put (M.insert name types env)
    pruneTypeSignatures terms
  _ -> do
    pruned <- pruneTypeSignatures terms
    return (term:pruned)
pruneTypeSignatures [] = return []
