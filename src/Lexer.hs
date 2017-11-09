{-# LANGUAGE OverloadedStrings #-}

module Lexer ( ErrorMsg(..)
             , Parser
             , ParseError
             , runLexer
             , runLexer_
             ) where

import           Control.Monad              (void)
import           Control.Monad.Trans.Class
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as BS
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.Foldable              (asum)
import           Data.List                  (intercalate)
import           Data.Monoid                ((<>))
import           System.IO
import           Text.Megaparsec            hiding (ParseError)
import           Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as L



import           CLangDef
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

integerConstant :: Parser m ByteString
integerConstant = lexeme $ (C8.pack . show) <$> integer -- TODO: improve this


charConstant :: Parser m ByteString
charConstant = lexeme $ do
  _ <- asum [string "u\'", string "U\'", string "l\'", string "\'"]
  x <- simpleEscapeSequence <|>
       BS.singleton <$> noneOf [w '\\', w '\'', w '\n'] <|>
       fail "empty character constant"
  _ <- char $ w '\''
  return x




simpleEscapeSequence :: Parser m ByteString
simpleEscapeSequence = do -- refer to 'simple escape sequence'
    c1 <- char (w '\\')
    c2 <- oneOf (BS.unpack "\\\"'?abfnrtv") <?> "valid escape sequence"
    return $ BS.pack [c1,c2]


stringLiteral :: Parser m ByteString
stringLiteral = lexeme $ do
  _ <- asum $ map string ["u8\"", "u\"", "U\"", "L\"", "\""]
  s <- many sChar
  _ <- char $ w '\"'
  return $ BS.concat s

-- | parses an s-char (see 6.4.5)
sChar :: Parser m ByteString
sChar = allowedCharacter <|>
        simpleEscapeSequence
  where
    allowedCharacter = BS.singleton <$> noneOf [w '\\', w '"', w '\n']

identifierOrKeyword :: Parser m ByteString
identifierOrKeyword =  do
  x <- letterChar <|> char (w '_')
  y <- many (alphaNumChar <|> char (w '_'))
  return $  BS.pack (x : y)

identifier :: Parser m ByteString
identifier = lexeme $ try $ do
  name <- identifierOrKeyword
  if name `elem` allCKeywords
  then fail "not an identifier"
  else return name

keyword :: Parser m ByteString
keyword = lexeme $ try $ do
  name <- identifierOrKeyword
  if name `elem` allCKeywords
  then return name
  else fail "not a keyword"

punctuator :: Parser m ByteString
punctuator = lexeme $ asum $ map string allCPunctuators


cToken :: Parser m CToken
cToken = DecConstant <$> integerConstant <|>
         CharConstant <$> charConstant <|>
         StringLit <$> stringLiteral <|>
         Identifier <$> identifier <|>
         Keyword <$> keyword <|>
         Punctuator <$> punctuator

-- " parses a cToken and immediately outputs it in IO and discards the result"
cToken_ :: Parser IO ()
cToken_ =  do
      p <- getPosition
      msg <- (charConstant     >>= \s -> return $ "constant " <> s) <|>
             (integerConstant  >>= \s -> return $ "constant " <> s)  <|>
             (keyword          >>= \s -> return $ "keyword " <> s)  <|>
             (identifier       >>= \s -> return $ "identifier " <> s)  <|>
             (stringLiteral    >>= \s -> return $ "string-literal " <> s)  <|>
             (punctuator       >>= \s -> return $ "punctuator " <> s)
      lift $ C8.putStr (prettyPrintPos p)
      lift $ C8.putStr ": "
      lift $ C8.putStrLn msg

lexer :: Parser m [(CToken, SourcePos)]
lexer = setTabWidth pos1 >>  sc *> many posCToken <* eof
  where posCToken = do
          p <- getPosition
          t <- cToken
          return (t,p)


runLexer :: String -> ByteString -> Either ParseError [(CToken, SourcePos)]
runLexer = runParser lexer


-- | this tokenizer just outputs directly to stdout and discards any tokens.
-- | (this one has basically no use other than benchmarking)
runLexer_ :: String -> ByteString -> IO (Either ParseError ())
runLexer_ = runParserT $  do
  setTabWidth pos1
  lift $ hSetBuffering stdout (BlockBuffering Nothing) -- much faster this way
  sc
  _ <- many cToken_
  eof
  lift $ hFlush stdout


prettyPrintPos :: SourcePos -> ByteString
prettyPrintPos s = C8.pack $ intercalate ":" [sourceName s
                                             , show $ unPos $ sourceLine s
                                             , show $ unPos $ sourceColumn s]
