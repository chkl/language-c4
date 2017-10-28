{-# LANGUAGE OverloadedStrings #-}

module Lexer where
import           Control.Monad.Identity
import           Text.Parsec.Char
import           Text.Parsec.Pos                          (SourcePos)
import           Text.Parsec.Prim                         (Parsec, getPosition,
                                                           try, (<|>))
import           Text.Parsec.String
import qualified Text.Parsec.Token                        as T
import           Text.ParserCombinators.Parsec.Combinator

import           CLangDef


data Token = Keyword String
           | Identifier String
           | DecConstant Integer
           | CharConstant Char
           | StringLit String
           | Punctuator String
           deriving (Show, Eq)


tokenParser :: T.GenTokenParser String u Identity
tokenParser = T.makeTokenParser cLangDef

-- | Takes a parser and transforms it into a parser that first consumes all
-- | whitespace, then reads the position and then return its original result
-- | paired with that position
withPosition :: Parsec String u a -> Parsec String u (a, SourcePos)
withPosition p = do
  _ <- T.whiteSpace tokenParser
  pos <- getPosition
  a <- p
  return (a, pos)


integerConstantToken :: Parsec String u (Token, SourcePos)
integerConstantToken = withPosition $ DecConstant <$> T.integer tokenParser

charConstantToken :: Parsec String u (Token, SourcePos)
charConstantToken = withPosition $ do
  _ <- optional (oneOf "uUL" )
  _ <- char '\''
  c <- anyChar
  _ <- char '\''
  return $ CharConstant c

stringLiteralToken :: Parsec String u (Token, SourcePos)
stringLiteralToken = withPosition $ StringLit <$> T.stringLiteral tokenParser

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


