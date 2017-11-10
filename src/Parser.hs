{-# LANGUAGE OverloadedStrings #-}

module Parser where


import           Control.Monad              (void)
import           Control.Monad.Trans.Class
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as BS
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.Foldable              (asum)
import           Data.List                  (groupBy, sortBy)
import           Data.Ord                   (comparing)
import           System.IO
import           Text.Megaparsec            hiding (ParseError)
import           Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as L


import           CLangDef                   (w)
import qualified Lexer                      as L
import           Types

identifier :: Parser m Expr
identifier = ExprIdent <$> L.identifier

constP :: Parser m Expr
constP = Constant <$> (L.integerConstant <|> L.charConstant)

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

chainr1 :: Parser m a -> Parser m (a -> a -> a) -> Parser m a
chainr1 p op = do
  x <- p
  let recurse = do
        o <- op
        y <- chainr1 p op
        return $ x `o` y
  recurse <|> return x

-- TODO: rewrite this
chainl1 :: Parser m a -> Parser m (a -> a -> a) -> Parser m a
chainl1 p op = p >>= rest
  where rest x = do f <- op
                    y <- p
                    rest (f x y)
                 <|> return x

unaryOp :: Parser m Expr
unaryOp = identifier


binaryOp :: Parser m Expr
binaryOp =  binaryOp' operators

binaryOp' :: [[BOperator m]]-> Parser m Expr
binaryOp' [] = unaryOp
binaryOp' (o:ops) =
        case associativity (head o) of
          LeftAssoc  -> chainl1 (binaryOp' ops) (eqPrecedence o)
          RightAssoc -> chainr1 (binaryOp' ops) (eqPrecedence o)
  where
    eqPrecedence :: [BOperator m] -> Parser m (Expr -> Expr -> Expr)
    eqPrecedence ops' = asum $ map opParser ops'


operators :: [[BOperator m]]
operators = groupBy eqPrec $  (sortBy (flip $ comparing precedence)) $
            [ BOperator LeftAssoc Plus  (L.stringLexeme "+" >> return (BExpr Plus)) 4
            , BOperator LeftAssoc Minus (L.stringLexeme "-" >> return (BExpr Minus)) 4
            , BOperator LeftAssoc Mult (L.stringLexeme "*" >> return (BExpr Mult)) 2
            ]
  where eqPrec o1 o2 = precedence o1 == precedence o2

