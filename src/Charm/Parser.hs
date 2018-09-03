module Charm.Parser where

import Data.Char
import Data.List
import Text.Read

data Token =
      TSpace                 -- ' '
    | TOpenBracket           -- [
    | TCloseBracket          -- ]
    | TString                -- "
    | TAlternate             -- |
    | TSignature             -- ::
    | TDefinition            -- :=
    | TNumber String         -- /-?\d+(\.\d+)?/ (using readMaybe)
    | TIdentifier String     -- any other string

type Code = [ASTCode]
data ASTCode = 
      ASTList [ASTCode]
    | ASTNumber String
    | ASTString String
    | ASTFunction String
    | ASTDefinition String [ASTCode]

data TypeSignature = TypeSignature String [ASTTypeSignatureUnit] [ASTTypeSignatureUnit]
--                                 Name   Popped                 Pushed 
data ASTTypeSignatureUnit = 
      TSUType String
    | TSUTypeVariable String
    | TSUTypeUnion [ASTTypeSignatureUnit]
    | TSUList [ASTTypeSignatureUnit]


lexOneToken :: String -> Maybe (Token, String)
lexOneToken line@(c:cs)
    | c == ' '     = Just (TSpace, cs)
    | c == '['     = Just (TOpenBracket, cs)
    | c == ']'     = Just (TCloseBracket, cs)
    | c == '"'     = Just (TString, cs)
    | c == '|'     = Just (TAlternate, cs)
    | word == "::" = Just (TSignature, rest)
    | word == ":=" = Just (TDefinition, rest)
    | otherwise = case (readMaybe word :: Maybe Double) of
        Just _  -> Just (TNumber word, rest)
        Nothing -> Just (TIdentifier word, rest)
    where
        (word, rest) = break isSpace line
lexOneToken "" = Nothing

lexOneLine :: String -> [Token]
lexOneLine = unfoldr lexOneToken

lex :: String -> [[Token]]
lex = map lexOneLine . lines

parseOneLine :: [Token] -> Either TypeSignature Code
parseOneLine line =
  
    where
    

parse :: [[Token]] -> [Either TypeSignature Code]
parse = map parseOneLine
