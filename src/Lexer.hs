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


integerConstantToken :: Parsec String u (Token, SourcePos)
integerConstantToken = do
  i <- T.integer tokenParser
  p <- getPosition
  return (DecConstant i, p)

charConstantToken :: Parsec String u (Token, SourcePos)
charConstantToken = do
  _ <- optional (oneOf "uUL" )
  _ <- char '\''
  c <- anyChar
  _ <- char '\''
  p <- getPosition
  return (CharConstant c, p)

stringLiteralToken :: Parsec String u (Token, SourcePos)
stringLiteralToken = do
  _ <- T.whiteSpace tokenParser -- somehow the generated stringLiteral will not accept whitespaces
  s <- T.stringLiteral tokenParser
  p <- getPosition
  return (StringLit s, p)

identifierToken :: Parsec String u (Token, SourcePos)
identifierToken = do
  s <- T.identifier tokenParser
  p <- getPosition
  return (Identifier s, p)

punctuatorToken :: Parsec String u (Token, SourcePos)
punctuatorToken = do
  _ <- T.whiteSpace tokenParser -- somehow the generated punctuatorToken will not accept whitespaces
  s <- choice $ map (try.string) allCPunctuators
  p <- getPosition
  return (Punctuator s, p)


keywordToken :: Parsec String u (Token, SourcePos)
keywordToken = do
  s <- choice $ map (\ s -> (T.reserved tokenParser s >> return s)) allCKeywords
  p <- getPosition
  return (Keyword s, p)

lexer :: Parser [(Token, SourcePos)]
lexer = manyTill ( try integerConstantToken <|>
                   try  stringLiteralToken <|>
                   try  charConstantToken <|>
                   try  keywordToken <|>
                   try  identifierToken <|>
                   try  punctuatorToken
                 ) eof


