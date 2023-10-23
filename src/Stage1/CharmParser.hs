{-# LANGUAGE BlockArguments #-}

module Stage1.CharmParser where

import Text.Megaparsec
import Text.Megaparsec.Debug
import Text.Megaparsec.Char
import Data.Ratio
import Data.Void
import Control.Monad
import Data.Char
import Data.Maybe (isJust, fromJust)

type Lexer = Parsec Void String

data CharmTypeTerm =
  CharmType String
  | CharmTypeQ String
  | CharmTypeVar String
  deriving (Show, Eq, Ord)

data T = T [CharmTypeTerm] [CharmTypeTerm] deriving (Show, Eq)

data CharmTerm =
  CharmIdent String
  | CharmNumber Rational
  | CharmString String
  | CharmList [CharmTerm]
  | CharmTypeSig String T
  | CharmDef String [CharmTerm]
  deriving (Show, Eq)

-- Let's begin by lexing the input string into tokens
data CharmToken = 
  TokenNumDecimal String String
  | TokenNumInteger String 
  | TokenQuote String -- We take the whole string as a token to apply different escaping rules
  | TokenBareWord String
  -- Simple Tokens (without extra data)
  | TokenColon -- :
  | TokenAssign -- :=
  | TokenEndAssign -- ;
  | TokenOpenParen -- (
  | TokenCloseParen -- )
  | TokenOpenBracket -- [
  | TokenCloseBracket -- ]
  | TokenArrow -- -> 
  | TokenAlternative -- |
  | TokenVariadic -- ...
  deriving (Show, Eq, Ord)

tokenDecimal :: Lexer CharmToken 
tokenDecimal = do
  neg <- many $ char '-'
  pre <- some digitChar
  char '.'
  post <- some digitChar
  return $ TokenNumDecimal (neg ++ pre) post 

tokenInteger :: Lexer CharmToken
tokenInteger = do
  neg <- many $ char '-'
  num <- some digitChar
  return . TokenNumInteger $ neg ++ num

tokenBareWord :: Lexer CharmToken
tokenBareWord = TokenBareWord <$> some (satisfy (\c -> or [isAlphaNum c,isPunctuation c, isSymbol c]))

tokenQuote :: Lexer CharmToken
tokenQuote = do
  char '"'
  str <- many $ noneOf ['\\', '"'] <|> (char '\\' >> anySingle)
  char '"'
  return . TokenQuote $ str

tokenSimple :: Lexer CharmToken
tokenSimple = 
      (string ":="  >> return TokenAssign)
  <|> (char ':'   >> return TokenColon)
  <|> (char ';'     >> return TokenEndAssign)
  <|> (char '('     >> return TokenOpenParen)
  <|> (char ')'     >> return TokenCloseParen)
  <|> (char '['     >> return TokenOpenBracket)
  <|> (char ']'     >> return TokenCloseBracket)
  <|> (string "->"  >> return TokenArrow)
  <|> (char '|'     >> return TokenAlternative)
  <|> (string "..." >> return TokenVariadic)

charmTokens :: Lexer [CharmToken]
charmTokens = many $ foldr1 (<|>) (chompSpaces <$> parserList)
  where
    parserList = [tokenDecimal, tokenInteger, tokenQuote, tokenSimple, tokenBareWord]
    chompSpaces parser = space >> try parser <* space

-- Now let's turn those tokens into an AST
type Parser = Parsec Void [CharmToken]

data CharmAST = 
  -- Data level syntax
  ASTName String
  | ASTNumber Rational
  | ASTQuote String
  | ASTNest [CharmAST]
  | ASTDefinition String [CharmAST]
  -- Type level syntax
  | ASTTypeNest [CharmAST] [CharmAST]
  | ASTTypeAssignment String [CharmAST] [CharmAST]
  | ASTTypeVariadic CharmAST
  | ASTTypeAlternative [CharmAST] [CharmAST]
  deriving (Show, Eq)

consumeExtracted :: (CharmToken -> Maybe a) -> Parser a
consumeExtracted extract = satisfy (isJust . extract) >>= (return . fromJust . extract)

astName :: Parser CharmAST
astName = ASTName <$> consumeExtracted (\case { 
  (TokenBareWord s) -> Just s;
  _ -> Nothing })

astNest :: Parser CharmAST
astNest = ASTNest <$> between (single TokenOpenBracket) (single TokenCloseBracket) (many astTerm)

-- Parse a single data level Charm term 
astTerm :: Parser CharmAST
astTerm = foldr1 (<|>) (try <$> parserList)
  where
    astNumber :: Parser Rational
    astNumber = consumeExtracted (\case { 
      (TokenNumDecimal n d) -> let
        top = (read n :: Integer) % 1
        bot = (read d :: Integer) % (10 * (fromIntegral $ length d))
        in
          Just $ top + bot;
      (TokenNumInteger s) -> Just ((read s :: Integer) % 1);
      _ -> Nothing
      }) 
    astQuote :: Parser String
    astQuote = consumeExtracted (\case { 
      (TokenQuote s) -> Just s;
      _ -> Nothing 
      })
    parserList :: [Parser CharmAST]
    parserList = [astNest, ASTNumber <$> astNumber, ASTQuote <$> astQuote, astName]

astTypeNest :: Parser CharmAST
astTypeNest = between (single TokenOpenBracket) (single TokenCloseBracket) $ do
  precond <- many astTypeTerm
  single TokenArrow 
  postcond <- many astTypeTerm 
  return (ASTTypeNest precond postcond)

astTypeAlternative :: Parser CharmAST
astTypeAlternative = between (single TokenOpenParen) (single TokenCloseParen) $ do
  left <- many astTypeTerm
  single TokenAlternative
  right <- many astTypeTerm 
  return (ASTTypeAlternative left right)

-- Parse a single type level Charm term 
astTypeTerm :: Parser CharmAST 
astTypeTerm = foldr1 (<|>) (try <$> parserList)
  where 
    parserList :: [Parser CharmAST]
    parserList = [astName, astTypeNest]

astTypeAssignment :: Parser CharmAST
astTypeAssignment = do
  name <- astName
  single TokenColon
  precond <- many astTypeTerm
  single TokenArrow 
  postcond <- many astTypeTerm 
  single TokenEndAssign
  return (ASTTypeAssignment (\case { ASTName s -> s } $ name) precond postcond)
  
astDefinition :: Parser CharmAST
astDefinition = do
  name <- astName
  single TokenAssign
  body <- many astTerm
  single TokenEndAssign
  return $ ASTDefinition (\case { ASTName s -> s } $ name) body

charmAST :: Parser [CharmAST]
charmAST = many $ foldr1 (<|>) (try <$> [astDefinition, astTypeAssignment, astTerm])

data CharmParsingError = 
  CharmParsingLexError (ParseErrorBundle String Void)
  | CharmParsingParseError (ParseErrorBundle [CharmToken] Void)
  deriving (Show)
runCharmParser ::
     String -- Name of the source file
  -> String -- Parser input
  -> Either CharmParsingError [CharmAST] -- Successful parse
runCharmParser n i = 
  case (runParser charmTokens n i) of
    Left lexError -> Left $ CharmParsingLexError lexError
    Right parsedTokens -> 
      case (runParser charmAST n parsedTokens) of
        Left parseError -> Left $ CharmParsingParseError parseError
        Right parsedAst -> Right parsedAst
