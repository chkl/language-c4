{-# LANGUAGE OverloadedStrings #-}

module Parser where


import           Data.ByteString.Lazy (ByteString)
import           Data.Foldable        (asum)
import           Data.List            (groupBy, sortBy)
import           Data.Ord             (comparing)
import           Text.Megaparsec      hiding (ParseError)
import           Text.Megaparsec.Byte


import           CLangDef             (w)
import qualified Lexer                as L
import           Types


runParser :: String -> ByteString -> Either ParseError [ExternalDeclaration]
runParser = Text.Megaparsec.runParser translationUnit

--------------------------------------------------------------------------------
-- Root Parsers
--------------------------------------------------------------------------------

translationUnit :: Parser m [ExternalDeclaration]
translationUnit = L.sc >> many externalDeclaration <* eof

externalDeclaration :: Parser m ExternalDeclaration
externalDeclaration = try (ExtDeclarationFunction <$> functionDefinition) <|>
                      ExtDeclarationDeclaration <$> declaration

functionDefinition :: Parser m FunctionDefinition
functionDefinition = FunctionDefinition <$> typeSpecifier <*> declarator <*> compoundStatement

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
parenExpr = L.parens expression

primaryExpr :: Parser m Expr
primaryExpr = identifier <|>  constant <|> stringLit <|> parenExpr

--------------------------------------------------------------------------------
-- PostExpr Parsers
--------------------------------------------------------------------------------
postExpressions :: Expr -> Parser m Expr
postExpressions identExpr =     (L.punctuator "["  >> Array identExpr <$> expression <* L.punctuator "]")
                            <|> (L.punctuator "."  >> FieldAccess identExpr <$> identifier)
                            <|> (L.punctuator "->" >> PointerAccess identExpr <$> identifier)
                            <|> (L.punctuator "("  >> Func identExpr <$> expression  <* L.punctuator ")")
                            <|> (L.punctuator "("  >> L.punctuator ")" >> return (Func identExpr (List [])))

postExprNext :: Expr -> Parser m Expr
postExprNext expr = rest expr
    where rest e = do nextE <- postExpressions e
                      rest nextE <|> return nextE

postExpr' :: Parser m Expr
postExpr' = do
  expr <- firstPostExpr
  postExprNext expr <|> return expr
  where firstPostExpr = primaryExpr >>= postExpressions

postExpr :: Parser m Expr
postExpr =  try postExpr' <|> primaryExpr


--------------------------------------------------------------------------------
-- UnaryExpr Parsers
--------------------------------------------------------------------------------
uOp :: Parser m UOp
uOp = (L.keyword "sizeof" >> return SizeOf)  <|>
      (L.punctuator "&"   >> return Address) <|>
      (L.punctuator "*"   >> return Deref)   <|>
      (L.punctuator "-"   >> return Neg)     <|>
      (L.punctuator "!"   >> return Not)

unaryOp :: Parser m Expr
unaryOp = UExpr <$> uOp <*> unaryExpr

unaryExpr :: Parser m Expr
unaryExpr = try unaryOp <|> postExpr

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

binaryExpr :: Parser m Expr
binaryExpr =  binaryExpr' operators

binaryExpr' :: [[BOperator m]]-> Parser m Expr
binaryExpr' [] = unaryExpr
binaryExpr' (o:ops) =
        case associativity (head o) of
          LeftAssoc  -> chainl1 (binaryExpr' ops) (eqPrecedence o)
          RightAssoc -> chainr1 (binaryExpr' ops) (eqPrecedence o)
  where
    eqPrecedence :: [BOperator m] -> Parser m (Expr -> Expr -> Expr)
    eqPrecedence ops' = asum $ map opParser ops'


operators :: [[BOperator m]]
operators = groupBy eqPrec $  (sortBy (flip $ comparing precedence)) $
            [ BOperator LeftAssoc Plus  (L.punctuator "+" >> return (BExpr Plus)) 4
            , BOperator LeftAssoc Minus (L.punctuator "-" >> return (BExpr Minus)) 4
            , BOperator LeftAssoc Mult  (L.punctuator "*" >> return (BExpr Mult)) 2
            , BOperator LeftAssoc Minus (L.punctuator "<" >> return (BExpr LessThan)) 6
            , BOperator LeftAssoc EqualsEquals (L.punctuator "==" >> return (BExpr EqualsEquals)) 7
            , BOperator LeftAssoc NotEqual (L.punctuator "!=" >> return (BExpr NotEqual)) 7
            , BOperator LeftAssoc LAnd (L.punctuator "&&" >> return (BExpr LAnd)) 11
            , BOperator LeftAssoc LOr (L.punctuator "||" >> return (BExpr LOr)) 12
            , BOperator LeftAssoc AssignOp (L.punctuator "=" >> return (BExpr AssignOp)) 14
            ]
  where eqPrec o1 o2 = precedence o1 == precedence o2

--------------------------------------------------------------------------------
-- Expr Parsers
------------------------------------------------------------------------------
expression :: Parser m Expr
expression = listElim <$> (List <$> L.commaSep1 assignmentExpr)
  where listElim (List [x]) = x
        listElim exprs = exprs

assignmentExpr :: Parser m Expr
assignmentExpr = conditionalExpression <|>
  Assign <$> unaryExpr <* L.punctuator "=" <*> assignmentExpr

-- | writing this monadically is better than using alternatives as this avoid
-- very long backtracking for ternary operators.
-- TODO: Maybe some kind of chainl/r would make this nicer too?
conditionalExpression :: Parser m Expr
conditionalExpression = do
  x <- binaryExpr
  y <- optional $ do
    L.punctuator "?"
    e1 <- expression
    L.punctuator ":"
    e2 <- conditionalExpression
    return (e1,e2)
  case y of
    Nothing -> return x
    Just (e1,e2) -> return $ Ternary x e1 e2

--------------------------------------------------------------------------------
-- Statement Parsers
--------------------------------------------------------------------------------

statement :: Parser m Stmt
statement = ((L.keyword "break" >> return Break <* sem) <|>
            (L.keyword "continue" >> return Continue <* sem) <|>
            (L.keyword "return" >> (Return <$> optional expression) <* sem) <|>
            (L.keyword "goto" >> (Goto <$> L.identifier) <* sem) <|>
            (L.keyword "while" >> (WhileStmt <$> L.parens expression <*> statement)) <|>
            (L.keyword "if" >> (IfStmt <$> L.parens expression <*> statement <*> optional elseParser)) <|>
            (ExpressionStmt <$> optional expression <* sem) <|>
            compoundStatement <|>
            (LabeledStmt <$> L.identifier <* L.punctuator ":" <*> statement))
  where
    sem = L.punctuator ";"
    elseParser = L.keyword "else" >> statement

compoundStatement :: Parser m Stmt
compoundStatement = L.braces (CompoundStmt <$> many blockitem)
  where blockitem = (Left <$> declaration) <|> (Right <$> statement)

--------------------------------------------------------------------------------
-- Declaration Parsers
--------------------------------------------------------------------------------
typeSpecifier :: Parser m Type
typeSpecifier = (L.keyword "void"    >> return Void)  <|>
                (L.keyword "char"    >> return Char) <|>
                (L.keyword "int"     >> return Int)  <|>
                (L.keyword "struct"  >> structSpecifier)

structSpecifier :: Parser m Type
structSpecifier = try structInline <|> try structIdentifier
  where
    structIdentifier = StructIdentifier <$> L.identifier
    structInline = StructInline <$> optional L.identifier <*> L.braces structDeclarationList
    structDeclarationList = many structDeclaration

structDeclaration :: Parser m StructDeclaration
structDeclaration = StructDeclaration <$> typeSpecifier <*> L.commaSep declarator <* L.punctuator ";"

declarator :: Parser m Declarator
declarator = do
  pts <- many $ L.punctuator "*"
  Declarator (length pts) <$> directDeclarator


directDeclarator :: Parser m DirectDeclarator
directDeclarator = try pId <|>  pParent
  where
    pId = DirectDeclaratorId <$> L.identifier <*> many parameterList
    pParent = DirectDeclaratorParens <$> L.parens declarator <*> many parameterList


declaration :: Parser m Declaration
declaration = Declaration <$> typeSpecifier  <*>  initDeclaratorList <* L.punctuator ";"
  where initDeclaratorList = L.commaSep initDeclarator

initDeclarator :: Parser m InitDeclarator
initDeclarator = do
  d <- declarator
  ini <- optional $ L.punctuator "=" >> initializer -- this is simplified (not spec)
  return $ InitializedDec d ini

initializer :: Parser m Initializer
initializer = InitializerList <$> initList <|>
              InitializerAssignment <$> assignmentExpr
  where
    initList = L.braces (L.commaSep initializer <* optional L.comma)


parameterList :: Parser m [Parameter]
parameterList = L.parens $ L.commaSep parameterDeclaration

parameterDeclaration :: Parser m Parameter
parameterDeclaration = Parameter <$> typeSpecifier <*> declarator
