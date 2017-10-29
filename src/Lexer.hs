{-# LANGUAGE OverloadedStrings #-}

module Lexer where
import           Control.Monad.Identity
import           Text.Parsec.Char
import           Text.Parsec.Pos                          (SourcePos)
import           Text.Parsec.Prim
import           Text.Parsec.String
import qualified Text.Parsec.Token                        as T
import           Text.ParserCombinators.Parsec.Combinator

import           CLangDef

-- TODO: (Question) Choose more appropriate internal representations? e.g. Vector 4 Byte for CharConstant?
data Token = Keyword String
           | Identifier String
           | DecConstant Integer
           | CharConstant Char
           | StringLit String
           | Punctuator String
           deriving (Show, Eq)

-- | like char but discards the parsed character
char_ :: Char -> Parsec String u ()
char_ c = void $ char c

tokenParser :: T.GenTokenParser String u Identity
tokenParser = T.makeTokenParser cLangDef

-- | Takes a parser and transforms it into a parser that first consumes all
-- | whitespace, then reads the position and then return its original result
-- | paired with that position
withPosition :: Parsec String u a -> Parsec String u (a, SourcePos)
withPosition p = do
  T.whiteSpace tokenParser
  pos <- getPosition
  a <- p
  T.whiteSpace tokenParser
  return (a, pos)


integerConstantToken :: Parsec String u (Token, SourcePos)
integerConstantToken = withPosition $ DecConstant <$> T.integer tokenParser

charConstantToken :: Parsec String u (Token, SourcePos)
charConstantToken = withPosition $ do
  optional (oneOf "uUL" )
  char_ '\''
  c <- cChar -- for full compliance this should be many
  char_ '\''
  return $ CharConstant c

-- | parses a c-char (see 6.4.4.4)
cChar :: Parsec String u Char
cChar = try allowedCharacter <|> try simpleEscapeSequence where
  allowedCharacter = satisfy $ \c -> c `notElem` ['\\', '\'', '\n']

-- | parses an s-char (see 6.4.5)
sChar :: Parsec String u Char
sChar = try allowedCharacter <|> try simpleEscapeSequence where
  allowedCharacter = satisfy $ \c -> c `notElem` ['\\', '"', '\n']


simpleEscapeSequence :: Parsec String u Char
simpleEscapeSequence = do -- refer to 'simple escape sequence'
    char_ '\\'
    c <- oneOf $ map fst cSimpleEscapeSequences
    case lookup c cSimpleEscapeSequences of
      Just d  -> return d
      Nothing -> unexpected  "not a valid escape sequence"


stringLiteralToken :: Parsec String u (Token, SourcePos)
stringLiteralToken = withPosition $ do
  optional $ choice $ map string ["u8", "u", "U", "L"]
  char_ '\"'
  s <- many sChar
  char_ '\"'
  return $ StringLit s

identifierToken :: Parsec String u (Token, SourcePos)
identifierToken = withPosition $ Identifier <$> T.identifier tokenParser

punctuatorToken :: Parsec String u (Token, SourcePos)
punctuatorToken = withPosition $ Punctuator <$> choice (map (try.string) allCPunctuators)

keywordToken :: Parsec String u (Token, SourcePos)
keywordToken = withPosition $ Keyword <$> choice (map (\ s -> (T.reserved tokenParser s >> return s)) allCKeywords)



lexer :: Parser [(Token, SourcePos)]
lexer = manyTill ( try integerConstantToken <|>
                   try  stringLiteralToken <|>
                   try  charConstantToken <|>
                   try  keywordToken <|>
                   try  identifierToken <|>
                   try  punctuatorToken
                 ) eof


