{-# LANGUAGE OverloadedStrings #-}

module Lexer ( ErrorMsg(..)
             , CToken(..)
             , Parser
             , ParseError
             , runLexer
             , runLexer_
             ) where

import           Control.Monad              (void)
import           Control.Monad.Trans.Class
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as BS
import qualified Data.ByteString.Lazy.Char8       as C8
import           Data.Foldable              (asum)
import           Text.Megaparsec            hiding (ParseError)
import           Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as L



import           CLangDef
import           PrettyPrint
import           Types



-- | "space consumer"
sc :: Parser m ()
sc = L.space space1 lineCmnt blockCmnt
  where
--    lineCmnt  = L.skipLineComment "//"
    lineCmnt  =  lineCmntC
    blockCmnt = L.skipBlockComment "/*" "*/"


lineCmntC :: Parser m ()
lineCmntC = do
      _ <- string "//"
      let justChar = void $ satisfy (\c -> c `notElem` [w '\n', w '\\'] )
          justBackslash = char (w '\\') >> notFollowedBy (char (w '\n'))
      _ <- many ( justChar  <|>
                  (void.string) "\\\n" <|>
                  try justBackslash)
      return ()
--      void newline

-- `lexeme`, `integer` and `signedInteger` are basically pre-defined parsers of
-- megaparsec.

lexeme :: Parser m a -> Parser m a
lexeme = L.lexeme sc

integer :: Parser m Integer
integer = lexeme L.decimal

--  wrap any parser in `posLexeme` to make it consume any trailing whitespace
--  after the actual token Part of our conventions should be that every cToken
--  parser consumes all trailing whitespace.
--  It also embellishes the type a with the source position.
posLexeme :: Parser m a -> PosParser m a
posLexeme a = do
  p <- getPosition
  x <- lexeme a
  return (x,p)

integerConstant :: PosParser m CToken
integerConstant = posLexeme $ DecConstant <$> integer


charConstant :: PosParser m CToken
charConstant = posLexeme $ do
  _ <- asum [string "u\'", string "U\'", string "l\'", string "\'"]
  x <- simpleEscapeSequence <|>
       BS.singleton <$> noneOf [w '\\', w '\'', w '\n'] <|>
       fail "empty character constant"
  _ <- char $ w '\''
  return $ CharConstant x




simpleEscapeSequence :: Parser m ByteString
simpleEscapeSequence = do -- refer to 'simple escape sequence'
    c1 <- char (w '\\')
    c2 <- oneOf (BS.unpack "\\\"'?abfnrtv") <?> "valid escape sequence"
    return $ BS.pack [c1,c2]


stringLiteral :: PosParser m CToken
stringLiteral = posLexeme $ do
  _ <- asum $ map string ["u8\"", "u\"", "U\"", "L\"", "\""]
  s <- many sChar
  _ <- char $ w '\"'
  return $ StringLit $ BS.concat s

-- | parses an s-char (see 6.4.5)
sChar :: Parser m ByteString
sChar = allowedCharacter <|>
        simpleEscapeSequence
  where
    allowedCharacter = BS.singleton <$> noneOf [w '\\', w '"', w '\n']

identifierOrKeywordToken :: PosParser m CToken
identifierOrKeywordToken = posLexeme $ try $ do
  x <- letterChar <|> char (w '_')
  y <- many (alphaNumChar <|> char (w '_'))
  let name = BS.pack (x : y)
  if name `elem` allCKeywords
  then return $ Keyword name
  else return $ Identifier name


punctuatorToken :: PosParser m CToken
punctuatorToken = posLexeme $ do
  pun <- asum $ map string allCPunctuators
  return $ Punctuator pun

cToken :: PosParser m CToken
cToken = integerConstant <|>
         charConstant <|>
         stringLiteral <|>
         identifierOrKeywordToken <|>
         punctuatorToken

lexer :: Parser m [(CToken, SourcePos)]
lexer = setTabWidth pos1 >>  sc *> many cToken <* eof


runLexer :: String -> ByteString -> Either ParseError [(CToken, SourcePos)]
runLexer = runParser lexer

-- | this tokenizer just outputs directly to stdout and discards any tokens.
-- | (this one has basically no use other than benchmarking)
runLexer_ :: String -> ByteString -> IO (Either ParseError ())
runLexer_ = runParserT $  do
  setTabWidth pos1
  sc
  _ <- many $ do
    (t,p) <- cToken
    lift $ C8.putStr (prettyPrint p)
    lift $ C8.putStr ": "
    lift $ C8.putStrLn $ prettyPrint t
  eof

