module Main where

import Stage1.CharmParser
import Stage2.TypeChecker
import Stage3.CharmRuntime
import Text.Megaparsec
import Control.Monad.State
import Control.Monad
import Control.Monad.Trans.Except
import qualified Data.Map.Strict as M

getUntilEmptyLine :: IO String
getUntilEmptyLine = do
  input <- getLine
  case input of
    "" -> return input
    s -> liftM (\ss -> s ++ "\n" ++ ss) $ getUntilEmptyLine

main :: IO ()
main = do
  input <- getUntilEmptyLine
  -- Stage 1
  case parse parseCharm "interactive" input of
    Left bundle -> putStrLn (errorBundlePretty bundle)
    Right term -> do
      putStrLn "Parsed as:"
      putStrLn $ show term
      putStrLn ""
      putStrLn "Type signatures:"
      putStrLn . show $ execState (pruneTypeSignatures term) M.empty
      -- Stage 2
      -- TODO: don't do this like a nested case statement
      let (prunedCode, typeEnv) = runState (pruneTypeSignatures term) defaultEnv
      case check typeEnv prunedCode of
        Left err -> do
          putStrLn "Type Error!"
          putStrLn err
        Right _ -> do
          putStrLn "Type check success!"
          --Stage 3
          result <- runExceptT $ runCharm prunedCode defaultRuntime
          case result of
            Left err -> do
              putStrLn "Runtime Error!"
              putStrLn err
            Right _ -> return ()
      
      
