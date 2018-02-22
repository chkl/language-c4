{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module Language.C4.Parser
  ( parse
  , translationUnit
  , declaration
  , functionDefinition
  , statement
  , declarator
  , typeSpecifier
  , structDeclaration
  , postfixUnaryExpr
  , expression
  , unaryExpr
  , binaryExpr
  ) where


import           Data.ByteString        (ByteString)
import           Data.Foldable          (asum)
import           Data.List              (groupBy, sortBy)
import           Data.Ord               (comparing)
import           Text.Megaparsec        hiding (ParseError, SourcePos, parse)

import           Language.C4.Ast.SynAst
import qualified Language.C4.Lexer      as L
import           Language.C4.Types


parse :: (Monad m) => FilePath -> ByteString -> C4T m (TranslationUnit SynPhase)
parse fn s = case Text.Megaparsec.runParser (setTabWidth pos1 >> translationUnit) fn s of
                   Left err  -> throwC4 err
                   Right ast -> return ast


--------------------------------------------------------------------------------
-- Root Parsers
--------------------------------------------------------------------------------

-- | translates a file to a syntactic ast. 
translationUnit :: Parser m (TranslationUnit SynPhase)
translationUnit = L.sc >> TranslationUnit () <$>  some externalDeclaration <* eof

-- | either parses a functionDefinition or declaration
externalDeclaration :: Parser m (ExternalDeclaration SynPhase)
externalDeclaration =     try (Right <$> functionDefinition)
                      <|> Left <$> declaration

-- | parses a function definition consisting of position, type specifier, declarator and a compound statement.
functionDefinition :: Parser m (FunctionDefinition SynPhase)
functionDefinition = FunctionDefinition <$> getPosition <*> typeSpecifier <*> declarator <*> compoundStatement

--------------------------------------------------------------------------------
-- PrimaryExpr Parsers
--------------------------------------------------------------------------------

-- | parses either an expression, string, int or char constant, or identifier with position
primaryExpr :: Parser m (Expr SynPhase)
primaryExpr =     L.parens expression
              <|> stringLit
              <|> intconstant
              <|> charconstant
              <|> identifier
  where
    stringLit = StringLiteral <$> getPosition <*> L.stringLiteral
    intconstant = IntConstant <$> getPosition <*> L.integerConstant
    charconstant = CharConstant <$> getPosition <*> L.charConstant

--------------------------------------------------------------------------------
-- UnaryExpr Parsers
--------------------------------------------------------------------------------

-- | parses unary pre- or postfix expression.
unaryExpr :: Parser m (Expr SynPhase)
unaryExpr = prefixUnaryExpr <|> postfixUnaryExpr

-- | parses an expression with sizeof or a unary operator.
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

-- | parses possible unary postfix expressions. 
postfixUnaryExpr :: Parser m (Expr SynPhase)
postfixUnaryExpr = chainl1unary primaryExpr postElem
  where postElem = getPosition >>= \p ->
              (L.punctuator "["  >> flip (ArrayAccess p) <$> expression <* L.punctuator "]")
          <|> (L.punctuator "."  >> flip (FieldAccess p) <$> L.identifier)
          <|> (L.punctuator "->" >> flip (PointerAccess p) <$> L.identifier)
          <|> (L.punctuator "("  >> flip (Func p) <$> expressionList  <* L.punctuator ")")
          -- <|> (L.punctuator "("  >> L.punctuator ")" >> return (flip (Func p) (List [])))

-- | parses different prefix unary operators
uOp :: Parser m UOp
uOp = (L.keyword "sizeof" >> return SizeOf)  <|>
      (L.punctuator "&"   >> return Address) <|>
      (L.punctuator "*"   >> return Deref)   <|>
      (L.punctuator "-"   >> return Neg)     <|>
      (L.punctuator "!"   >> return Not)

--------------------------------------------------------------------------------
-- BinaryExpr Parsers
--------------------------------------------------------------------------------

-- | parses binary expression and applies chain functions with regard to the associativity.
binaryExpr :: Parser m (Expr SynPhase)
binaryExpr =  binaryExpr' operators
  where
    binaryExpr' :: [[BOperator m]] -> Parser m (Expr SynPhase)
    binaryExpr' [] = unaryExpr
    binaryExpr' (o:ops) =
            case associativity (head o) of
              LeftAssoc  -> chainl1 (binaryExpr' ops) (eqPrecedence o)
              RightAssoc -> chainr1 (binaryExpr' ops) (eqPrecedence o)
      where
        eqPrecedence :: [BOperator m] -> Parser m (Expr SynPhase -> Expr SynPhase -> Expr SynPhase)
        eqPrecedence ops' = asum $ map opParser ops'

data Associativity = LeftAssoc | RightAssoc

data BOperator m = BOperator { associativity :: Associativity
                             , opParser      :: Parser m (Expr SynPhase -> Expr SynPhase -> Expr SynPhase)
                             , precedence    :: Int
                             }

-- | describes the different binary operators with associativity and precedence.
operators :: [[BOperator m]]
operators = groupBy eqPrec $  sortBy (flip $ comparing precedence)
            [ BOperator LeftAssoc (mk Plus "+") 4
            , BOperator LeftAssoc (mk Minus "-") 4
            , BOperator LeftAssoc (mk Mult "*") 2
            , BOperator LeftAssoc (mk LessThan "<") 6
            , BOperator LeftAssoc (mk EqualsEquals "==") 7
            , BOperator LeftAssoc (mk NotEqual "!=") 7
            , BOperator LeftAssoc (mk LAnd "&&") 11
            , BOperator LeftAssoc (mk LOr "||") 12
            , BOperator LeftAssoc (mk AssignOp "=") 14
            ]
  where eqPrec o1 o2 = precedence o1 == precedence o2
        mk op pun = do
          p <- getPosition
          L.punctuator pun
          return $ BExpr p op

--------------------------------------------------------------------------------
-- Expr Parsers
------------------------------------------------------------------------------

-- | parses a list of comma separated expressions.
expressionList = listElim . List <$> L.commaSep expression
  where listElim (List [x]) = x
        listElim exprs      = exprs

-- | parses either a binary or ternary expression consisting.
expression :: Parser m (Expr SynPhase)
expression = do
  p <- getPosition
  x <- binaryExpr
  y <- optional $ do
    L.punctuator "?"
    e1 <- expression
    L.punctuator ":"
    e2 <- expression
    return (e1,e2)
  case y of
    Nothing      -> return x
    Just (e1,e2) -> return $ Ternary p x e1 e2

--------------------------------------------------------------------------------
-- Statement Parsers
--------------------------------------------------------------------------------

-- | parses a statement like break, continue, return, goto, while, if or a compound, expression or labeled statement.
statement :: Parser m (Stmt SynPhase)
statement =  getPosition >>= \p -> (L.keyword "break" >> return (Break p) <* sem)
                               <|> (L.keyword "continue" >> return (Continue p) <* sem)
                               <|> (L.keyword "return" >> (Return p <$> optional expression) <* sem)
                               <|> (L.keyword "goto" >> (Goto p <$> L.identifier) <* sem)
                               <|> (L.keyword "while" >> (WhileStmt p <$> L.parens expression <*> statement))
                               <|> (L.keyword "if" >> (IfStmt p <$> L.parens expression <*> statement <*> optional elseParser))
                               <|> compoundStatement
                               <|> try (ExpressionStmt <$> getPosition <*> expressionList <* sem)
                               <|> (LabeledStmt p <$> L.identifier <* L.punctuator ":" <*> statement)
  where
    sem = L.punctuator ";"
    elseParser = L.keyword "else" >> statement

-- | is a helper function combing two parsers
parseEither :: Parser m a -> Parser m b -> Parser m (Either a b)
parseEither pa pb = try (Left <$> pa) <|> Right <$> pb

-- | parses a compound statement by using L.braces.
compoundStatement :: Parser m (Stmt SynPhase)
compoundStatement = do
  p <- getPosition
  x <- L.braces $ many (parseEither declaration statement)
  return $ CompoundStmt p x

--------------------------------------------------------------------------------
-- Declaration Parsers
--------------------------------------------------------------------------------

-- | parses either void, char, int or struct.
typeSpecifier :: Parser m (Type SynPhase)
typeSpecifier =
  getPosition >>= \p ->
  (    L.keyword "void"    >> return (Void p))
  <|> (L.keyword "char"    >> return (Char p))
  <|> (L.keyword "int"     >> return (Int p))
  <|> (L.keyword "struct"  >> structSpecifier)

-- | parses a either an inline struct or an struct identifier.
structSpecifier :: Parser m (Type SynPhase)
structSpecifier = try structInline <|> structIdentifier
  where
    structIdentifier = StructIdentifier <$> getPosition <*> L.identifier
    structInline = StructInline <$> getPosition <*> optional L.identifier <*> L.braces structDeclarationList
    structDeclarationList = many structDeclaration

-- | parses a struct declaration with a comma separated list.
structDeclaration :: Parser m (StructDeclaration SynPhase)
structDeclaration = StructDeclaration <$> getPosition <*> typeSpecifier <*> L.commaSep declarator <* L.punctuator ";"

-- | is like a direct declarator but with many * in front.
declarator :: Parser m (Declarator SynPhase)
declarator = do
  p <- getPosition
  x <- optional (L.punctuator "*")
  case x of
    Nothing -> directDeclarator
    _       -> IndirectDeclarator p <$> declarator

-- | parses a direct declarator using chainl1unary.
directDeclarator :: Parser m (Declarator SynPhase)
directDeclarator = chainl1unary dcore dparams
  where
    dcore =   L.parens declarator
              <|> (DeclaratorId <$> getPosition <*> L.identifier)
    dparams = do
      p <- getPosition
      try (flip (FunctionDeclarator p) <$> parameterList) <|>
          flip (ArrayDeclarator p) <$> L.brackets expression

-- | works as declarator but for abstract ones. 
abstractDeclarator :: Parser m (AbstractDeclarator SynPhase)
abstractDeclarator =  do
  x <- optional (L.punctuator "*")
  case x of
    Nothing -> directAbstractDeclarator
    _       -> IndirectAbstractDeclarator <$> getPosition <*> abstractDeclarator

-- | works as directDeclarator but for abstract ones.
directAbstractDeclarator :: Parser m (AbstractDeclarator SynPhase)
directAbstractDeclarator = chainl1unary core ops
  where
    core = L.parens abstractDeclarator <|> (AbstractTerminal <$> getPosition)
    ops = getPosition >>= \p -> flip (AbstractFunctionDeclarator p) <$> parameterList <|>
          L.brackets (L.punctuator "*" >> (ArrayStar <$> getPosition))

-- | parses a list of declarations using initDeclarator.
declaration :: Parser m (Declaration SynPhase)
declaration = Declaration <$> getPosition <*> typeSpecifier  <*>  initDeclaratorList <* L.punctuator ";"
  where initDeclaratorList = L.commaSep initDeclarator

-- | parses an initialized declarator.
initDeclarator :: Parser m (InitDeclarator SynPhase)
initDeclarator = do
  d <- declarator
  ini <- optional $ L.punctuator "=" >> initializer -- this is simplified (not spec)
  return $ InitializedDec d ini

-- | parses either an initializer list or an initializer assignment
initializer :: Parser m (Initializer SynPhase)
initializer = InitializerList <$> initList <|>
              (InitializerAssignment <$> expression)
  where
    initList = L.braces (L.commaSep initializer <* optional L.comma)

-- | parses a comma separated lists of parameter declarations.
parameterList :: Parser m [Parameter SynPhase]
parameterList = L.parens $ L.commaSep parameterDeclaration

-- | parses either a parameter or abstract parameter declaration.
parameterDeclaration :: Parser m (Parameter SynPhase)
parameterDeclaration = try x <|>  y
  where x = Parameter <$> getPosition <*> typeSpecifier <*> declarator
        y = AbstractParameter <$> getPosition <*> typeSpecifier <*> optional abstractDeclarator

-- | parses identifiers. 
identifier:: Parser m (Expr SynPhase)
identifier = ExprIdent <$> getPosition <*> L.identifier

--------------------------------------------------------------------------------
-- Useful combinators
--------------------------------------------------------------------------------

-- | TODO
chainr1 :: Parser m a -> Parser m (a -> a -> a) -> Parser m a
chainr1 p op = do
  x <- p
  let recurse = do
        o <- op
        y <- chainr1 p op
        return $ x `o` y
  recurse <|> return x

-- | TODO:
chainl1 :: Parser m a -> Parser m (a -> a -> a) -> Parser m a
chainl1 p op = p >>= rest
  where rest x = do f <- op
                    y <- p
                    rest (f x y)
                 <|> return x

-- | chainl1unary does the same as chainl but for unary operators.
chainl1unary :: Parser m a -> Parser m (a -> a) -> Parser m a
chainl1unary p op = p >>= rest
  where rest x = do f <- op
                    rest (f x)
                 <|> return x
