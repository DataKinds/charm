{-# LANGUAGE BlockArguments #-}

module Stage1.CharmParser where

import Text.Megaparsec
import Text.Megaparsec.Debug
import Text.Megaparsec.Char
import Data.Ratio
import Data.Void
import Control.Monad
import Data.Char

type Parser = Parsec Void String

data CharmTypeTerm =
  CharmType String
  | CharmTypeQ String
  | CharmTypeVar String
  deriving (Show, Eq)

data T = T [CharmTypeTerm] [CharmTypeTerm] deriving (Show, Eq)

data CharmTerm =
  CharmIdent String
  | CharmNumber Rational
  | CharmString String
  | CharmList [CharmTerm]
  | CharmTypeSig String T
  | CharmDef String [CharmTerm]
  deriving (Show, Eq)

-- the default `space` matches newlines, this one doesn't
space' :: Parser ()
space' = void $ takeWhileP (Just "white space") (\c -> isSpace c && c /= '\n')

parseCharm :: Parser [CharmTerm]
parseCharm = some parseAll

parseAll :: Parser CharmTerm
parseAll = foldr1 (<|>) $ try . between space space <$> [parseDef, parseTypeSig, parseList, parseString, parseNumber, parseIdent]

parseIdent :: Parser CharmTerm
parseIdent = some (try letterChar <|> try digitChar) >>= (return . CharmIdent)

parseNumber :: Parser CharmTerm
parseNumber = try parseDouble <|> try parseInteger
  where
    parseDouble :: Parser CharmTerm
    parseDouble = do
      pre <- some digitChar
      char '.'
      post <- some digitChar
      return . CharmNumber $ ((read pre :: Integer) % 1) + ((read post :: Integer) % (10 * (fromIntegral $ length post)))
      
    parseInteger :: Parser CharmTerm
    parseInteger = do
      num <- some digitChar
      return . CharmNumber $ (read num :: Integer) % 1

parseString :: Parser CharmTerm
parseString = do
  char '"'
  str <- many (satisfy (\c -> c /= '"'))
  char '"'
  return . CharmString $ str

parseList :: Parser CharmTerm
parseList = do
  char '['
  terms <- many parseAll
  char ']'
  return . CharmList $ terms
  
parseTypeTerm :: Parser CharmTypeTerm
parseTypeTerm = try parseTypeQ <|> try parseType <|> try parseTypeVar
  where
    parseTypeQ :: Parser CharmTypeTerm
    parseTypeQ = do
      term <- (try parseType <|> try parseTypeVar)
      string "?"
      return term

    parseType :: Parser CharmTypeTerm
    parseType = do
      head <- upperChar
      rest <- many alphaNumChar
      return $ CharmType (head : rest)

    parseTypeVar :: Parser CharmTypeTerm
    parseTypeVar = do
      (CharmIdent s) <- parseIdent
      return . CharmTypeVar $ s
  

-- TODO: parsing nested type signatures
parseTypeSig :: Parser CharmTerm
parseTypeSig = do
  (CharmIdent s) <- parseIdent
  space'
  string ":"
  pre <- (try $ many (between space' space' parseTypeTerm)) <|> (char ' ' *> return []) 
  string "->"
  post <- (try $ many (between space' space' parseTypeTerm)) <|> (space1 *> return []) 
  space
  return $ CharmTypeSig s (T pre post)

parseDef :: Parser CharmTerm
parseDef = dbg "Def" $ do
  (CharmIdent s) <- parseIdent
  space'
  string ":="
  space'
  def <- some parseAll
  space
  return $ CharmDef s def
