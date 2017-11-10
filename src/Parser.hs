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

--------------------------------------------------------------------------------
-- PrimaryExpr Parsers
--------------------------------------------------------------------------------

identifier :: Parser m Expr
identifier = ExprIdent <$> L.identifier

constant :: Parser m Expr
constant = Constant <$> (L.integerConstant <|> L.charConstant)

stringLit :: Parser m Expr
stringLit = StringLiteral <$> L.stringLiteral

parenExpr :: Parser m Expr
parenExpr = do
  char $ w '('
  expr <- expression
  char $ w ')'
  return expr

primaryExpr :: Parser m Expr
primaryExpr = identifier <|>  constant <|> stringLit <|> parenExpr

--------------------------------------------------------------------------------
-- PostExpr Parsers
--------------------------------------------------------------------------------
firstPostExpr :: Parser m Expr
firstPostExpr = do
  primary <- primaryExpr
  expr    <- postExpr' primary
  return expr

postExpr':: Expr -> Parser m Expr
postExpr' identExpr= do
  p <- L.punctuator
  case p of
    "["  -> expression >>= (\expr -> L.stringLexeme "]" >> return (Array identExpr expr))
    "."  -> identifier >>= (\ident -> return (FieldAccess identExpr ident))
    "->" -> identifier >>= (\ident -> return (PointerAccess identExpr ident))
    "("  -> ((L.stringLexeme ")" >> return (Func identExpr (List [])))
            <|> (expression >>= (\expr -> L.stringLexeme ")" >> return (Func identExpr expr))))
    _    -> fail "not a post expr"

postExprNext :: Expr -> Parser m Expr
postExprNext expr = rest expr
    where rest e = do nextE <- postExpr' e
                      rest nextE <|> return nextE

postExpr2 :: Parser m Expr
postExpr2 = do
  expr <- firstPostExpr
  postExprNext expr <|> return expr

-- TODO: investigate the use of "try" here. Should only use a lookahead of 2. 
-- This actually needs a lookahead of 3--but can fix by simply always
-- calling "primaryExpr" first, since both firstPostExpr and postExpr2
-- call primaryExpr first. 
postExpr :: Parser m Expr
postExpr =  try postExpr2 <|> try firstPostExpr <|> primaryExpr

--------------------------------------------------------------------------------
-- UnaryExpr Parsers
--------------------------------------------------------------------------------
uOp :: Parser m ByteString
uOp = L.stringLexeme "sizedof"
      <|> L.stringLexeme "&"
      <|> L.stringLexeme "*"
      <|> L.stringLexeme "-"
      <|> L.stringLexeme "!"

unary' :: ByteString -> Expr -> Parser m Expr
unary' uop expr = do
  case uop of
   "sizeof" -> return $ UExpr SizeOf expr
   "&"      -> return $ UExpr Address expr
   "*"      -> return $ UExpr Deref expr
   "-"      -> return $ UExpr Neg expr
   "!"      -> return $ UExpr Not expr

unaryOp1 :: Parser m Expr
unaryOp1 = scan
  where scan = uOp >>= rest
        rest op = (do expr <- scan
                      newExpr <- unary' op expr
                      return newExpr)
                  <|> (do expr <- postExpr
                          newExpr <- unary' op expr
                          return newExpr)

-- TODO: investigate the use of "try" here. Should only use a lookahead of 2. 
unaryOp :: Parser m Expr
unaryOp = try unaryOp1 <|> postExpr

--------------------------------------------------------------------------------
-- BinaryExpr Parsers
--------------------------------------------------------------------------------
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


--------------------------------------------------------------------------------
-- TernaryExpr Parsers
--------------------------------------------------------------------------------
ternary :: Parser m Expr
ternary = undefined
  

--------------------------------------------------------------------------------
-- Expr Parsers
--------------------------------------------------------------------------------
expression :: Parser m Expr
expression = undefined
