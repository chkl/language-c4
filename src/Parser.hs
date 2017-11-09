

module Parser where

import Lexer
import Types
import           Control.Monad              (void)
import           Control.Monad.Trans.Class
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as BS
import qualified Data.ByteString.Lazy.Char8       as C8
import           Data.Foldable              (asum)
import           Text.Megaparsec            hiding (ParseError)
import           Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as L
import System.IO


identP :: Parser m PrimaryExpr
identP = IdentifierExpr <$> identifier

constP :: Parser m PrimaryExpr
constP = ConstExpr <$> (integerConstant <|> charConstant)

stringP :: Parser m PrimaryExpr
stringP = StringExpr <$> stringLiteral 

parenExprP :: Parser PrimaryExpr
parenExprP = do
  char '('
  expr <- exprP
  char ')'
  return $ ParenExpr expr.

pExprP :: Parser m PrimaryExpr
pExprP = identP <|>  constP <|> stringP <|> parenExprP

exprP :: Parser m Expr
exprP = undefined
