{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module Parser where


import           Data.ByteString.Lazy (ByteString)
import           Data.Foldable        (asum)
import           Data.List            (groupBy, sortBy)
import           Data.Ord             (comparing)
import           Text.Megaparsec      hiding (ParseError, SourcePos)

import           Ast.SynAst
import qualified Lexer                as L
import           Types



runParser :: String -> ByteString -> Either ParseError (TranslationUnit SynPhase)
runParser = Text.Megaparsec.runParser translationUnit

--------------------------------------------------------------------------------
-- Root Parsers
--------------------------------------------------------------------------------

translationUnit :: Parser m (TranslationUnit SynPhase)
translationUnit = L.sc >> TranslationUnit () <$>  some externalDeclaration <* eof

externalDeclaration :: Parser m (ExternalDeclaration SynPhase)
externalDeclaration =     try (Right <$> functionDefinition)
                      <|> Left <$> declaration

functionDefinition :: Parser m (FunctionDefinition SynPhase)
functionDefinition = FunctionDefinition <$> getPosition <*> typeSpecifier <*> declarator <*> compoundStatement

--------------------------------------------------------------------------------
-- PrimaryExpr Parsers
--------------------------------------------------------------------------------

primaryExpr :: Parser m (Expr SynPhase)
primaryExpr =     L.parens expression
              <|> stringLit
              <|> constant
              <|> identifier
  where
    stringLit = StringLiteral <$> getPosition <*> L.stringLiteral
    constant = Constant <$> getPosition <*> (L.integerConstant <|> L.charConstant)

--------------------------------------------------------------------------------
-- UnaryExpr Parsers
--------------------------------------------------------------------------------

unaryExpr :: Parser m (Expr SynPhase)
unaryExpr = prefixUnaryExpr <|> postfixUnaryExpr

prefixUnaryExpr :: Parser m (Expr SynPhase)
prefixUnaryExpr =     try sizeofTypename
                  <|> sizeOfOtherwise
  where sizeofTypename = do
          p <- getPosition
          L.keyword "sizeof"
          t <- L.parens typeSpecifier
          return (SizeOfType p t)
        sizeOfOtherwise = do
          p <- getPosition
          u <- uOp
          e <- unaryExpr
          return (UExpr p u e)

postfixUnaryExpr :: Parser m (Expr SynPhase)
postfixUnaryExpr = chainl1unary primaryExpr postElem
  where postElem = getPosition >>= \p ->
              (L.punctuator "["  >> flip (Array p) <$> expression <* L.punctuator "]")
          <|> (L.punctuator "."  >> flip (FieldAccess p) <$> identifier)
          <|> (L.punctuator "->" >> flip (PointerAccess p) <$> identifier)
          <|> (L.punctuator "("  >> flip (Func p) <$> expression  <* L.punctuator ")")
          <|> (L.punctuator "("  >> L.punctuator ")" >> return (flip (Func p) (List [])))


uOp :: Parser m UOp
uOp = (L.keyword "sizeof" >> return SizeOf)  <|>
      (L.punctuator "&"   >> return Address) <|>
      (L.punctuator "*"   >> return Deref)   <|>
      (L.punctuator "-"   >> return Neg)     <|>
      (L.punctuator "!"   >> return Not)

--------------------------------------------------------------------------------
-- BinaryExpr Parsers
--------------------------------------------------------------------------------

binaryExpr :: Parser m (Expr SynPhase)
binaryExpr =  binaryExpr' operators
  where
    binaryExpr' :: [[BOperator m]]-> Parser m (Expr SynPhase)
    binaryExpr' [] = unaryExpr
    binaryExpr' (o:ops) =
            case associativity (head o) of
              LeftAssoc  -> chainl1 (binaryExpr' ops) (eqPrecedence o)
              RightAssoc -> chainr1 (binaryExpr' ops) (eqPrecedence o)
      where
        eqPrecedence :: [BOperator m] -> Parser m (Expr SynPhase -> Expr SynPhase -> Expr SynPhase)
        eqPrecedence ops' = asum $ map opParser ops'


data BOperator m = BOperator { associativity :: Associativity
                             , operatorP     :: BOp
                             , opParser      :: Parser m (Expr SynPhase -> Expr SynPhase -> Expr SynPhase)
                             , precedence    :: Int
                             }
operators :: [[BOperator m]]
operators = groupBy eqPrec $  sortBy (flip $ comparing precedence)
            [ BOperator LeftAssoc Plus  (mk Plus "+") 4
            , BOperator LeftAssoc Minus (mk Minus "-") 4
            , BOperator LeftAssoc Mult  (mk Mult "*") 2
            , BOperator LeftAssoc Minus (mk LessThan "<") 6
            , BOperator LeftAssoc EqualsEquals (mk EqualsEquals "==") 7
            , BOperator LeftAssoc NotEqual (mk NotEqual "!=") 7
            , BOperator LeftAssoc LAnd (mk LAnd "&&") 11
            , BOperator LeftAssoc LOr (mk LOr "||") 12
            , BOperator LeftAssoc AssignOp (mk AssignOp "=") 14
            ]
  where eqPrec o1 o2 = precedence o1 == precedence o2
        mk op pun = do
          p <- getPosition
          L.punctuator pun
          return $ BExpr p op

--------------------------------------------------------------------------------
-- Expr Parsers
------------------------------------------------------------------------------
expression :: Parser m (Expr SynPhase)
expression = listElim <$> (List <$> L.commaSep1 assignmentExpr)
  where listElim (List [x]) = x
        listElim exprs      = exprs

assignmentExpr :: Parser m (Expr SynPhase)
assignmentExpr = conditionalExpression <|>
  Assign <$> getPosition <*> unaryExpr <* L.punctuator "=" <*> assignmentExpr

-- | writing this monadically is better than using alternatives as this avoid
-- very long backtracking for ternary operators.
-- TODO: Maybe some kind of chainl/r would make this nicer too?
conditionalExpression :: Parser m (Expr SynPhase)
conditionalExpression = do
  p <- getPosition
  x <- binaryExpr
  y <- optional $ do
    L.punctuator "?"
    e1 <- expression
    L.punctuator ":"
    e2 <- conditionalExpression
    return (e1,e2)
  case y of
    Nothing      -> return x
    Just (e1,e2) -> return $ Ternary p x e1 e2

--------------------------------------------------------------------------------
-- Statement Parsers
--------------------------------------------------------------------------------

statement :: Parser m (Stmt SynPhase)
statement =  getPosition >>= \p -> (L.keyword "break" >> return (Break p) <* sem)
                               <|> (L.keyword "continue" >> return (Continue p) <* sem)
                               <|> (L.keyword "return" >> (Return p <$> optional expression) <* sem)
                               <|> (L.keyword "goto" >> (Goto p <$> L.identifier) <* sem)
                               <|> (L.keyword "while" >> (WhileStmt p <$> L.parens expression <*> statement))
                               <|> (L.keyword "if" >> (IfStmt p <$> L.parens expression <*> statement <*> optional elseParser))
                               <|> compoundStatement
                               <|> try (ExpressionStmt <$> getPosition <*> optional expression <* sem)
                               <|> (LabeledStmt p <$> L.identifier <* L.punctuator ":" <*> statement)
  where
    sem = L.punctuator ";"
    elseParser = L.keyword "else" >> statement

parseEither :: Parser m a -> Parser m b -> Parser m (Either a b)
parseEither pa pb = try (Left <$> pa) <|> Right <$> pb

compoundStatement :: Parser m (Stmt SynPhase)
compoundStatement = do
  p <- getPosition
  x <- L.braces $ many (parseEither declaration statement)
  return $ CompoundStmt p x

--------------------------------------------------------------------------------
-- Declaration Parsers
--------------------------------------------------------------------------------
typeSpecifier :: Parser m (Type SynPhase)
typeSpecifier =     (L.keyword "void"    >> return Void)
                <|> (L.keyword "char"    >> return Char)
                <|> (L.keyword "int"     >> return Int)
                <|> (L.keyword "struct"  >> structSpecifier)

structSpecifier :: Parser m (Type SynPhase)
structSpecifier = try structInline <|> structIdentifier
  where
    structIdentifier = StructIdentifier <$> L.identifier
    structInline = StructInline <$> optional L.identifier <*> L.braces structDeclarationList
    structDeclarationList = many structDeclaration

structDeclaration :: Parser m (StructDeclaration SynPhase)
structDeclaration = StructDeclaration <$> getPosition <*> typeSpecifier <*> L.commaSep declarator <* L.punctuator ";"

-- | like a direct declarator but with many * in front
declarator :: Parser m (Declarator SynPhase)
declarator = do
  p <- getPosition
  pts <- length <$> many (L.punctuator "*")
  if pts > 0 then
      IndirectDeclarator p pts <$> directDeclarator
    else
      directDeclarator


directDeclarator :: Parser m (Declarator SynPhase)
directDeclarator = chainl1unary dcore dparams
  where
    dcore =   L.parens declarator
              <|> (DeclaratorId <$> getPosition <*> L.identifier)
    dparams = getPosition >>= \p -> flip (FunctionDeclarator p) <$> parameterList

abstractDeclarator :: Parser m (AbstractDeclarator SynPhase)
abstractDeclarator =  do
  pts <- length <$> many (L.punctuator "*")
  if pts > 0 then
    IndirectAbstractDeclarator pts <$> directAbstractDeclarator
  else
    directAbstractDeclarator

directAbstractDeclarator :: Parser m (AbstractDeclarator SynPhase)
directAbstractDeclarator = chainl1unary core ops
  where
    core = L.parens abstractDeclarator
    ops = flip AbstractFunctionDeclarator <$> parameterList <|>
          L.brackets (L.punctuator "*" >> return ArrayStar)

declaration :: Parser m (Declaration SynPhase)
declaration = Declaration <$> getPosition <*> typeSpecifier  <*>  initDeclaratorList <* L.punctuator ";"
  where initDeclaratorList = L.commaSep initDeclarator

initDeclarator :: Parser m (InitDeclarator SynPhase)
initDeclarator = do
  d <- declarator
  ini <- optional $ L.punctuator "=" >> initializer -- this is simplified (not spec)
  return $ InitializedDec d ini

initializer :: Parser m (Initializer SynPhase)
initializer = InitializerList <$> initList <|>
              InitializerAssignment <$> assignmentExpr
  where
    initList = L.braces (L.commaSep initializer <* optional L.comma)


parameterList :: Parser m [Parameter SynPhase]
parameterList = L.parens $ L.commaSep parameterDeclaration

parameterDeclaration :: Parser m (Parameter SynPhase)
parameterDeclaration = try x <|>  y
  where x = Parameter <$> getPosition <*> typeSpecifier <*> declarator
        y = AbstractParameter <$> getPosition <*> typeSpecifier <*> optional abstractDeclarator


identifier :: Parser m (Expr SynPhase)
identifier = ExprIdent <$> getPosition <*> L.identifier

--------------------------------------------------------------------------------
-- Useful combinators
--------------------------------------------------------------------------------
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

-- | like  chainl but for unary operators.
chainl1unary :: Parser m a -> Parser m (a -> a) -> Parser m a
chainl1unary p op = p >>= rest
  where rest x = do f <- op
                    rest (f x)
                 <|> return x
