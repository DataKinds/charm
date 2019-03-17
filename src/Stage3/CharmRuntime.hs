module Stage3.CharmRuntime where

import Prelude
import qualified Data.Map as M
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Stage1.CharmParser
import Stage2.TypeChecker

data CharmValue =
  ValueFunction String
  | ValueNumber Rational
  | ValueString String
  | ValueList [CharmValue]

instance Show CharmValue where
  show (ValueFunction s) = s ++ ":="
  show (ValueNumber r) = show r
  show (ValueString s) = s
  show (ValueList l) = show l

termToValue :: CharmTerm -> CharmValue
termToValue (CharmIdent s)   = ValueFunction s
termToValue (CharmNumber n)  = ValueNumber n
termToValue (CharmString s)  = ValueString s
termToValue (CharmList ts)   = ValueList $ termToValue <$> ts
termToValue (CharmTypeSig _ _) = error "ERROR: couldn't coerce CharmTypeSig to value. You should never see this error!"
termToValue (CharmDef _ _)     = error "ERROR: couldn't coerce CharmDef to value. You should never see this error!"

data Runtime = Runtime {
  runtimeStack :: [CharmValue],
  runtimeDefs :: M.Map String (Either [CharmTerm] (Runtime -> IO Runtime))
}

register :: String -> (Runtime -> IO Runtime) -> Runtime -> Runtime
register fname f (Runtime stack defs) = Runtime stack (M.insert fname (Right f) defs)

define :: String -> [CharmTerm] -> Runtime -> Runtime
define fname f (Runtime stack defs) = Runtime stack (M.insert fname (Left f) defs)

runCharmTerm :: CharmTerm -> Runtime -> ExceptT String IO Runtime
runCharmTerm (CharmTypeSig _ _) r = return r
runCharmTerm (CharmDef fname f) r = return . define fname f $ r
runCharmTerm (CharmIdent s) r@(Runtime stack defs) = case M.lookup s defs of
  Just def -> case def of
    Left charmCode -> runCharm charmCode r
    Right native -> lift $ native r
  Nothing -> throwE $ "Couldn't find function of name " ++ s
runCharmTerm term (Runtime stack defs) = return $ Runtime ((termToValue term):stack) defs

runCharm :: [CharmTerm] -> Runtime -> ExceptT String IO Runtime
runCharm (t:ts) r = runCharmTerm t r >>= runCharm ts 
runCharm [] r = return r

--- PRELUDE FUNCTIONS ---
emptyRuntime :: Runtime
emptyRuntime = Runtime [] M.empty

defaultRuntime :: Runtime
defaultRuntime = populateRuntime emptyRuntime

populateRuntime :: Runtime -> Runtime 
populateRuntime =
  register "p"
    (\(Runtime (a:stack) defs) -> do
        print a
        return $ Runtime stack defs)...
  register "pstring"
    (\(Runtime (a:stack) defs) -> do
        putStrLn . show $ a
        return $ Runtime stack defs)...
  register "getline"
    (\(Runtime stack defs) -> do
        s <- getLine
        return $ Runtime ((ValueString s):stack) defs)
    where
      (...) = flip (.)
