{-# LANGUAGE OverloadedStrings #-}

module Lexer ( ErrorMsg(..)
             , CToken(..)
             , Parser
             , ParseError
             , runLexer
             ) where

import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import           Data.Foldable              (asum)
import           Data.Word                  (Word8)
import           Text.Megaparsec            hiding (ParseError)
import           Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as L
import qualified Text.Megaparsec.Error      as E



import           CLangDef

data CToken = Keyword ByteString
            | Identifier ByteString
            | DecConstant Integer
            | CharConstant Word8
            | StringLit ByteString
            | Punctuator ByteString
           deriving (Show, Eq)

newtype ErrorMsg = ErrorMsg { toString :: String
                         } deriving (Ord, Eq, Show)

instance ShowErrorComponent ErrorMsg where
  showErrorComponent e = "error: " ++ toString e

type ParseError = E.ParseError Word8 ErrorMsg

type Parser = Parsec ErrorMsg ByteString

type PosParser a = Parser (a, SourcePos)


-- | "space consumer"
sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

-- `lexeme`, `integer` and `signedInteger` are basically pre-defined parsers of
-- megaparsec.

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Integer
integer = lexeme L.decimal

signedInteger :: Parser Integer
signedInteger = L.signed sc integer

--  wrap any parser in `posLexeme` to make it consume any trailing whitespace
--  after the actual token Part of our conventions should be that every cToken
--  parser consumes all trailing whitespace.
--  It also embellishes the type a with the source position.
posLexeme :: Parser a -> PosParser a
posLexeme a = do
  p <- getPosition
  x <- lexeme a
  return (x,p)

integerConstant :: PosParser CToken
integerConstant = posLexeme $ DecConstant <$> integer


charConstant :: PosParser CToken
charConstant = posLexeme $ do
  _ <- optional $ oneOf [w 'u', w 'U', w 'l']
  _ <- char $ w '\''
  x <- try simpleEscapeSequence <|> noneOf [w '\\', w '\'', w '\n']
  _ <- char $ w '\''
  return $ CharConstant (x :: Word8)




simpleEscapeSequence :: Parser Word8
simpleEscapeSequence = do -- refer to 'simple escape sequence'
    _ <- char (w '\\')
    c <- oneOf $ map fst cSimpleEscapeSequences
    case lookup c cSimpleEscapeSequences of
      Just d  -> return d
      Nothing -> fail "not a valid escape sequence"


stringLiteral :: PosParser CToken
stringLiteral = posLexeme $ do
  _ <- optional $ asum $ map string ["u8", "u", "U", "L"]
  _ <- char $ w '\"'
  s <- many sChar
  _ <- char $ w '\"'
  return $ StringLit $ BS.pack s

-- | parses an s-char (see 6.4.5)
sChar :: Parser Word8
sChar = try allowedCharacter <|> try simpleEscapeSequence where
  allowedCharacter = noneOf [w '\\', w '"', w '\n']

identifierToken :: PosParser CToken
identifierToken = posLexeme $ do
  x <- letterChar <|> char (w '_')
  y <- many (alphaNumChar <|> char (w '_'))
  return $ Identifier (BS.pack (x : y))


punctuatorToken :: PosParser CToken
punctuatorToken = posLexeme $ do
  pun <- asum $ map string allCPunctuators
  return $ Punctuator pun

keywordToken :: PosParser CToken
keywordToken = posLexeme $ do
   k <- asum $ map string allCKeywords
   return $ Keyword (k :: ByteString )


cToken :: PosParser CToken
cToken = try integerConstant <|>
         try  stringLiteral <|>
         try  charConstant <|>
         try  keywordToken <|>
         try  identifierToken <|>
         try  punctuatorToken

fullLexer :: Parser [(CToken, SourcePos)]
fullLexer = sc *> many cToken <* eof


runLexer :: String -> ByteString -> Either ParseError [(CToken, SourcePos)]
runLexer = parse fullLexer



