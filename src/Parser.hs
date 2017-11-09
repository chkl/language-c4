

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

{-
termOpP :: OpTable -> OpTable -> ReadP Term
termOpP opTable restOpTable = do
  case restOpTable of
    OpTable [] -> baseP opTable
    OpTable (opEntry:ops) ->
        case opEntry of
          (FNone, operators) -> chainl1 (termOpP opTable (OpTable ops)) (thisLevelP operators)
          (FLeft, operators) -> chainl1 (termOpP opTable (OpTable ops)) (thisLevelP operators)
          (FRight, operators) -> chainr1 (termOpP opTable (OpTable ops)) (thisLevelP operators)

thisLevelP :: [String] -> ReadP (Term -> Term -> Term)
thisLevelP operators = do
    whitespaceP
    op <- choice $ map string operators
    case notValidOp op of
      True -> pfail
      False -> do whitespaceP
                  let applyOp = \termL termR -> TFun op [termL, termR]
                  return applyOp
    where notValidOp op = (op == "=") ||
                          (any (\c -> not (c `elem` ['!','@','#','+','-','*','/','\\','<','>','='])) op)

opTable1 =  OpTable
  [(FNone, ["<=", "<"]),
   (FLeft, ["+", "-"]),
   (FLeft, ["*"]),
   (FRight, ["**"])]



-}
