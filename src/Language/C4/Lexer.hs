{-# LANGUAGE OverloadedStrings #-}

module Language.C4.Lexer
  ( ErrorMsg(..)
  , Parser
  , ParseError
  , CToken(..)
  , runLexer
  , tokenize
  , identifier
  , charConstant
  , integerConstant
  , stringLexeme
  , keyword
  , anyKeyword
  , stringLiteral
  , anyPunctuator
  , punctuator
  , parens
  , braces
  , brackets
  , commaSep
  , commaSep1
  , semicolSep
  , comma
  , sc
  ) where


import           Control.Monad              (void)
import           Control.Monad.Trans.Class
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as C8
import           Data.ByteString.Short      as SBS
import           Data.Foldable              (asum)
import           Data.Monoid                ((<>))
import           System.IO
import           Text.Megaparsec            hiding (ParseError)
import           Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as MBL


import           Language.C4.CLangDef
import           Language.C4.Types


data CToken = Keyword ShortByteString
            | Identifier ShortByteString
            | DecConstant Integer
            | CharConstant ByteString
            | StringLit ByteString
            | Punctuator ByteString
           deriving (Show, Eq)


-- | "space consumer"
sc :: Parser m ()
sc = MBL.space space1 lineCmnt blockCmnt
  where
--    lineCmnt  = L.skipLineComment "//"
    lineCmnt  =  lineCmntC
    blockCmnt = MBL.skipBlockComment "/*" "*/"


lineCmntC :: Parser m ()
lineCmntC = do
      _ <- string "//"
      let justChar = void $ satisfy (\c -> c `notElem` [w '\r', w '\n', w '\\'] )
          justBackslash = char (w '\\') >> notFollowedBy (oneOf [w '\n', w '\r'])
      _ <- many ( justChar  <|>
                  (void.string) "\\\n" <|>
                  try justBackslash)
      return ()
--      void newline

-- `lexeme`, `integer` and `signedInteger` are basically pre-defined parsers of
-- megaparsec.

lexeme :: Parser m a -> Parser m a
lexeme = MBL.lexeme sc

symbol :: ByteString -> Parser m ByteString
symbol = MBL.symbol sc

parens :: Parser m a -> Parser m a
parens    = between (symbol "(") (symbol ")")

braces :: Parser m a -> Parser m a
braces    = between (symbol "{") (symbol "}")

brackets :: Parser m a -> Parser m a
brackets  = between (symbol "[") (symbol "]")

commaSep :: Parser m a -> Parser m [a]
commaSep p = p `sepBy` comma

commaSep1 :: Parser m a -> Parser m [a]
commaSep1 p = p `sepBy1` comma

comma :: Parser m ()
comma = void (symbol ",")

semicolSep :: Parser m a -> Parser m [a]
semicolSep p = p `sepBy` symbol ";"

integer :: Parser m Integer
integer = lexeme MBL.decimal

integerConstant :: Parser m Integer 
integerConstant = lexeme $ integer -- TODO: improve this


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

identifierOrKeyword :: Parser m ShortByteString
identifierOrKeyword =  do
  x <- letterChar <|> char (w '_')
  y <- many (alphaNumChar <|> char (w '_'))
  return $  SBS.pack (x : y)

identifier :: Parser m ShortByteString
identifier = lexeme $ try $ do
  name <- identifierOrKeyword
  if name `elem` allCKeywords
  then fail "not an identifier"
  else return name

anyKeyword :: Parser m ShortByteString
anyKeyword = lexeme $ try $ do
  name <- identifierOrKeyword
  if name `elem` allCKeywords
  then return name
  else fail "not a keyword"

anyPunctuator :: Parser m ByteString
anyPunctuator = lexeme $ asum $ map string allCPunctuators

{-# DEPRECATED stringLexeme "Use @keyword or @identifier instead" #-}
stringLexeme :: ByteString -> Parser m ByteString
stringLexeme = lexeme . string

keyword :: ByteString -> Parser m ()
keyword s = try $ void $ lexeme $ string s

punctuator :: ByteString -> Parser m ()
punctuator = void . lexeme . string

cToken :: Parser m CToken
cToken = DecConstant <$> integerConstant <|>
         CharConstant <$> charConstant <|>
         StringLit <$> stringLiteral <|>
         Identifier <$> identifier <|>
         Keyword <$> anyKeyword <|>
         Punctuator <$> anyPunctuator

-- " parses a cToken and immediately outputs it in IO and discards the result"
cToken_ :: Parser IO ()
cToken_ =  do
      p <- getPosition
      lift $ C8.putStr (prettyPrintPos p)
      lift $ C8.putStr ": "
      _ <-  (charConstant     >>= \s -> lift $ C8.putStrLn $ "constant '" <> s <> "'") <|>
            (integerConstant  >>= \s -> lift $ C8.putStrLn $ "constant " <> undefined )     <|>
            (anyKeyword       >>= \s -> lift $ C8.putStrLn $ "keyword " <> (fromShort s))  <|>
            (identifier       >>= \s -> lift $ C8.putStrLn $ "identifier " <> (fromShort s))  <|>
            (stringLiteral    >>= \s -> lift $ C8.putStrLn $ "string-literal \"" <> s <> "\"")  <|>
            (anyPunctuator       >>= \s -> lift $ C8.putStrLn $ "punctuator " <> s)
      return ()

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
tokenize :: FilePath -> ByteString -> IO (C4 ())
tokenize fn s = do
  res <- runParserT (do
    setTabWidth pos1
    lift $ hSetBuffering stdout (BlockBuffering Nothing) -- much faster this way
    sc
    _ <- many cToken_
    eof
    lift $ hFlush stdout
    ) fn s
  case res of
    Left err -> return $ throwC4 err
    Right () -> return $ return ()

{-# INLINABLE tokenize  #-}
{-# INLINABLE identifier #-}
{-# INLINABLE charConstant #-}
{-# INLINABLE integerConstant #-}
{-# INLINABLE stringLexeme #-}
{-# INLINABLE keyword #-}
{-# INLINABLE anyKeyword #-}
{-# INLINABLE stringLiteral #-}
{-# INLINABLE anyPunctuator #-}
{-# INLINABLE punctuator #-}
{-# INLINABLE parens #-}
{-# INLINABLE braces #-}
{-# INLINABLE brackets #-}
{-# INLINABLE commaSep #-}
{-# INLINABLE commaSep1 #-}
{-# INLINABLE semicolSep #-}
{-# INLINABLE comma #-}
{-# INLINABLE sc #-}
