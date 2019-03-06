module Stage3.CharmRuntime where

import qualified Data.Map as M
import Stage1.CharmParser
import Stage2.TypeChecker

data CharmValue =
  ValueFunction String
  | ValueNumber Rational
  | ValueString String
  | ValueList [CharmValue]

termToValue :: CharmTerm -> CharmValue
termToValue (CharmIdent s)   = ValueFunction s
termToValue (CharmNumber n)  = ValueNumber n
termToValue (CharmString s)  = ValueString s
termToValue (CharmList ts)   = ValueList $ termToValue <$> ts
termToValue (CharmTypeSig _ _) = error "ERROR: couldn't coerce CharmTypeSig to value. You should never see this error!"
termToValue (CharmDef _ _)     = error "ERROR: couldn't coerce CharmDef to value. You should never see this error!"

data Runtime = Runtime {
  runtimeStack :: [CharmValue],
  runtimeDefs :: M.Map String (Either [CharmValue] (Runtime -> Runtime))
}

register :: String -> (Runtime -> Runtime) -> Runtime -> Runtime
register fname f (Runtime stack defs) = Runtime stack (M.insert fname (Right f) defs)

define :: String -> [CharmTerm] -> Runtime -> Runtime
define fname f (Runtime stack defs) = Runtime stack (M.insert fname (Left $ termToValue <$> f) defs)


--- PRELUDE FUNCTIONS ---
emptyRuntime :: Runtime
emptyRuntime = Runtime [] M.empty

populateRuntime :: Runtime -> Runtime
populateRuntime = do
  register "p" (\r -> r)
  register "pstring" (\r -> r)
