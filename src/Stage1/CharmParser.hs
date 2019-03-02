{-# LANGUAGE BlockArguments #-}

module Stage1.CharmParser where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Ratio
import Data.Void

type Parser = Parsec Void String

data CharmTerm =
  CharmIdent String
  | CharmNumber Rational
  | CharmString String
  | CharmList [CharmTerm]
  | CharmTypeSig String [CharmTerm]
  | CharmDef String [CharmTerm]
  deriving (Show)

parseCharm :: Parser [CharmTerm]
parseCharm = many parseAll

parseAll :: Parser CharmTerm
parseAll = between (space) (space) $ choice [parseDef, parseTypeSig, parseList, parseIdent, parseNumber, parseString]

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
parseList = between (char '[') (char ']') (many parseAll) >>= (return . CharmList)

parseTypeSig :: Parser CharmTerm
parseTypeSig = do
  (CharmIdent s) <- parseIdent
  space
  string "::"
  space
  idents <- some parseOneType
  newline
  return $ CharmTypeSig s idents
    where
      parseOneType :: Parser CharmTerm
      parseOneType = do
        ident <- parseIdent
        space
        optional $ string "->"
        space
        return ident

parseDef :: Parser CharmTerm
parseDef = do
  (CharmIdent s) <- parseIdent
  space
  string ":="
  space
  def <- some parseAll
  newline
  return $ CharmDef s def
