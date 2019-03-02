module Main where

import Stage1.CharmParser
import Text.Megaparsec

main :: IO ()
main = do
  input <- getLine 
  case parse parseCharm "interactive" input of
    Left bundle -> putStrLn (errorBundlePretty bundle)
    Right term -> print term
