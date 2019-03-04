module Main where

import Stage1.CharmParser
import Stage2.TypeChecker
import Text.Megaparsec
import Control.Monad.State
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  input <- getLine 
  case parse parseCharm "interactive" input of
    Left bundle -> putStrLn (errorBundlePretty bundle)
    Right term -> do
      putStrLn "Parsed as:"
      putStrLn $ show term
      putStrLn ""
      putStrLn "Type signatures:"
      putStrLn . show $ execState (pruneTypeSignatures term) M.empty
      
      
