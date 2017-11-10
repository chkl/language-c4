

module Parser where


import           Control.Monad              (void)
import           Control.Monad.Trans.Class
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as BS
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.Foldable              (asum)
import           System.IO
import           Text.Megaparsec            hiding (ParseError)
import           Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as L


import           CLangDef                   (w)
import           Lexer
import           Types

identP :: Parser m Expr
identP = ExprIdent <$> identifier

constP :: Parser m Expr
constP = Constant <$> (integerConstant <|> charConstant)

-- stringP :: Parser m Expr
-- stringP = StringExpr <$> stringLiteral

parenExprP :: Parser m Expr
parenExprP = do
  char $ w '('
  expr <- exprP
  char $ w ')'
  return expr

-- pExprP :: Parser m Expr
-- pExprP = identP <|>  constP <|> stringP <|> parenExprP

exprP :: Parser m Expr
exprP = undefined

plusOp :: Parser m (Expr -> Expr -> Expr)
plusOp =  char (w '+') >> return (BExpr Plus)

-- just playing around with this
-- a = Expr
chainr1 :: Parser m a -> Parser m (a -> a -> a) -> Parser m a
chainr1 p op = do
  x <- p
  let recurse = do
        o <- op
        y <- chainr1 p op
        return $ x `o` y
  recurse <|> return x

chainl1 :: Parser m a -> Parser m (a -> a -> a) -> Parser m a
chainl1 p op = p >>= rest
  where rest x = do f <- op
                    y <- p
                    rest (f x y)
                 <|> return x

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
