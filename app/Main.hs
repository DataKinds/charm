module Main where

import Stage1.CharmParser
import Stage2.TypeChecker
import Stage3.CharmRuntime
import Control.Monad.State
import Control.Monad.Trans.Writer.CPS
import Control.Monad.Except
import Control.Monad
import Control.Monad.Trans.Except
import Data.Either
import Data.Functor.Identity
import Data.Maybe
import qualified Data.Map.Strict as M

getUntilEmptyLine :: IO String
getUntilEmptyLine = do
  input <- getLine
  case input of
    "" -> return input
    s -> liftM (\ss -> s ++ "\n" ++ ss) $ getUntilEmptyLine

-- | A pretty wrapper for Stage2.TypeChecker.extractTypesFromAST.
-- | Strips type signatures from an AST and returns them in a TypeEnvironment.
extractTypes :: [CharmAST] -> Except [CharmTypeError] (TypeEnvironment, [CharmAST])
extractTypes asts = do
  let typeEnvAction = runWriterT (extractTypesFromAST asts) 
  let ((nonTypeAST, errs), typeEnv) = runState typeEnvAction M.empty
  if errs == [] then
    -- The AST parsed into real type objects successfully!
    pure (typeEnv, nonTypeAST)
  else
    -- Something weird happened when trying to convert the AST into real types
    throwError (TypeErrorFatal . Just <$> errs)

typecheckOne 
  :: TypeEnvironment  -- The function binding environment we are typechecking in
  -> CharmAST -- The AST term to typecheck
  -> State [CharmType] (Maybe CharmTypeError) -- State is the stack state, result is any errors that occured
typecheckOne typeEnv (ASTNumber num) = modify ((TypeConcrete "Num"):) >> return Nothing
typecheckOne typeEnv (ASTQuote str) = modify ((TypeConcrete "Str"):) >> return Nothing
typecheckOne typeEnv (ASTName func) = case typeEnv M.!? func of
  Nothing -> return . Just $ TypeErrorUnknownFunction func
  Just (funcPops, funcPushes) -> do -- modify (funcPushes ++) >> return Nothing
    stackState <- get
    let (unifyResult, unifyTypeVars) = runState (runExceptT $ unify stackState funcPops) M.empty
        concFuncPushes = evalState (sequence $ concretize <$> funcPushes) unifyTypeVars
    case unifyResult of
      -- We don't know about this function! Let's error.
      Left typeError -> return . Just $ typeError
      -- We know about this function! Let's push its results onto the stack.
      Right postStack -> modify (concFuncPushes ++) >> return Nothing
typecheckOne typeEnv (ASTDefinition funcName funcDef) = case typeEnv M.!? funcName of
  Nothing -> undefined -- TODO: support type inference, if this lookup fails
  Just (funcPops, funcPushes) -> let
    -- We got a known type signature for this function! Let's check the function body.
    (unifyResult, unifyTypeVars) = runState (runExceptT $ unify funcPops funcPushes) M.empty
    in case unifyResult of
      -- The function body didn't match the type signature
      Left typeError -> return . Just $ typeError
      -- The function body either matched the type signature, or pushed some extra stuff onto the stack.
      Right postStack -> return $ if length postStack > 0 then Just $ TypeErrorPushedTooMuch postStack else Nothing
typecheckOne typeEnv (ASTNest _) = undefined
typecheckOne typeEnv ast = let
  errMsg = "Unknown AST node " ++ (show ast) ++ " in typecheck! extractTypes failed to strip type data?"
  in return . Just . TypeErrorFatal . Just $ errMsg

typecheck 
  :: [CharmAST]  
  -> Except [CharmTypeError] [CharmType]  -- Either a typechecking error or the output of the whole program
typecheck asts = do
  (typeEnv, nonTypeAST) <- extractTypes asts
  let
    typecheckOne' = typecheckOne typeEnv
    (errSeq, stackState) = runState (sequence $ typecheckOne' <$> asts) []
    errs = catMaybes errSeq
  if length errs == 0 then
    pure stackState
  else
    throwError errs

main :: IO ()
main = do
  let preludeFilename = "standard_lib/prelude.charm"
  contents <- readFile preludeFilename
  putStrLn "=============================="
  putStrLn $ "Reading from Prelude at " ++ preludeFilename
  putStrLn "------------------------------"
  putStrLn contents
  putStrLn "=============================="
  ast <- either (\err -> ioError $ userError (show err)) pure $ runCharmParser "<prelude>" contents
  putStrLn "Parsed the following AST"
  print ast
  putStrLn "=============================="
  let ((prunedAST, errs), typeEnv) = runIdentity $ runTypeContext (runWriterT $ extractTypesFromAST ast)
  when ((length errs) > 0) (ioError $ userError (show errs))
  putStrLn "Parsed the following type signatures"
  print typeEnv
  putStrLn "Pared down the following AST"
  print prunedAST
-- main = do
--   input <- getUntilEmptyLine
--   -- Stage 1
--   case parse charmAST "interactive" input of
--     Left bundle -> putStrLn (errorBundlePretty bundle)
--     Right term -> do
--       putStrLn "Parsed as:"
--       putStrLn $ show term
--       putStrLn ""
--       putStrLn "Type signatures:"
--       putStrLn . show $ execState (pruneTypeSignatures term) M.empty
--       -- Stage 2
--       -- TODO: don't do this like a nested case statement
--       let (prunedCode, typeEnv) = runState (pruneTypeSignatures term) defaultEnv
--       case check typeEnv prunedCode of
--         Left err -> do
--           putStrLn "Type Error!"
--           putStrLn err
--         Right _ -> do
--           putStrLn "Type check success!"
--           --Stage 3
--           result <- runExceptT $ runCharm prunedCode defaultRuntime
--           case result of
--             Left err -> do
--               putStrLn "Runtime Error!"
--               putStrLn err
--             Right _ -> return ()
      
      
