{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module Parser where


import           Data.ByteString.Lazy (ByteString)
import           Data.Foldable        (asum)
import           Data.List            (groupBy, sortBy)
import           Data.Ord             (comparing)
import           Text.Megaparsec      hiding (ParseError)


import qualified Lexer                as L
import           Types


--------------------------------------------------------------------------------
-- Type-FU
--------------------------------------------------------------------------------
data SynAnn

type instance AnnTranslationUnit SynAnn    =  ()
type instance AnnFunctionDefinition SynAnn =  SourcePos
type instance AnnTernary SynAnn            =  SourcePos
type instance AnnAssign SynAnn             =  SourcePos
type instance AnnArray SynAnn              =  SourcePos
type instance AnnBExpr SynAnn              =  SourcePos
type instance AnnUExpr SynAnn              =  SourcePos
type instance AnnFunc SynAnn               =  SourcePos
type instance AnnSizeOfType SynAnn         =  SourcePos
type instance AnnExprIdent SynAnn          =  SourcePos
type instance AnnConstant SynAnn           =  SourcePos
type instance AnnFieldAccess SynAnn        =  SourcePos
type instance AnnPointerAccess SynAnn      =  SourcePos
type instance AnnStringLiteral SynAnn      =  SourcePos
type instance AnnDeclaration SynAnn        =  SourcePos
type instance AnnIndirectDeclarator SynAnn =  SourcePos
type instance AnnStructDeclaration SynAnn  =  SourcePos
type instance AnnDeclaratorId SynAnn       =  SourcePos
type instance AnnFunctionDeclarator SynAnn =  SourcePos
type instance AnnCompoundStmt SynAnn       =  SourcePos
type instance AnnIfStmt SynAnn             =  SourcePos
type instance AnnWhileStmt SynAnn          =  SourcePos
type instance AnnGoto SynAnn               =  SourcePos
type instance AnnContinue SynAnn           =  SourcePos
type instance AnnBreak SynAnn              =  SourcePos
type instance AnnReturn SynAnn             =  SourcePos
type instance AnnLabeledStmt SynAnn        =  SourcePos


runParser :: String -> ByteString -> Either ParseError (TranslationUnit SynAnn)
runParser = Text.Megaparsec.runParser translationUnit

--------------------------------------------------------------------------------
-- Root Parsers
--------------------------------------------------------------------------------

translationUnit :: Parser m (TranslationUnit SynAnn)
translationUnit = L.sc >> TranslationUnit () <$>  some externalDeclaration <* eof

externalDeclaration :: Parser m (ExternalDeclaration SynAnn)
externalDeclaration =     try (Right <$> functionDefinition)
                      <|> Left <$> declaration

functionDefinition :: Parser m (FunctionDefinition SynAnn)
functionDefinition = FunctionDefinition <$> getPosition <*> typeSpecifier <*> declarator <*> compoundStatement

--------------------------------------------------------------------------------
-- PrimaryExpr Parsers
--------------------------------------------------------------------------------

primaryExpr :: Parser m (Expr SynAnn)
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

unaryExpr :: Parser m (Expr SynAnn)
unaryExpr = prefixUnaryExpr <|> postfixUnaryExpr

prefixUnaryExpr :: Parser m (Expr SynAnn)
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

postfixUnaryExpr :: Parser m (Expr SynAnn)
postfixUnaryExpr = chainl1unary primaryExpr postElem
  where postElem = getPosition >>= \p ->
              (L.punctuator "["  >> Array p <$> expression <* L.punctuator "]")
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

binaryExpr :: Parser m (Expr SynAnn)
binaryExpr =  binaryExpr' operators
  where
    binaryExpr' :: [[BOperator m]]-> Parser m (Expr SynAnn)
    binaryExpr' [] = unaryExpr
    binaryExpr' (o:ops) =
            case associativity (head o) of
              LeftAssoc  -> chainl1 (binaryExpr' ops) (eqPrecedence o)
              RightAssoc -> chainr1 (binaryExpr' ops) (eqPrecedence o)
      where
        eqPrecedence :: [BOperator m] -> Parser m (Expr SynAnn -> Expr SynAnn -> Expr SynAnn)
        eqPrecedence ops' = asum $ map opParser ops'


data BOperator m = BOperator { associativity :: Associativity
                             , operatorP     :: BOp
                             , opParser      :: Parser m (Expr SynAnn -> Expr SynAnn -> Expr SynAnn)
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
expression :: Parser m (Expr SynAnn)
expression = listElim <$> (List <$> L.commaSep1 assignmentExpr)
  where listElim (List [x]) = x
        listElim exprs      = exprs

assignmentExpr :: Parser m (Expr SynAnn)
assignmentExpr = conditionalExpression <|>
  Assign <$> getPosition <*> unaryExpr <* L.punctuator "=" <*> assignmentExpr

-- | writing this monadically is better than using alternatives as this avoid
-- very long backtracking for ternary operators.
-- TODO: Maybe some kind of chainl/r would make this nicer too?
conditionalExpression :: Parser m (Expr SynAnn)
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

statement :: Parser m (Stmt SynAnn)
statement =  getPosition >>= \p -> (L.keyword "break" >> return (Break p) <* sem)
                               <|> (L.keyword "continue" >> return (Continue p) <* sem)
                               <|> (L.keyword "return" >> (Return p <$> optional expression) <* sem)
                               <|> (L.keyword "goto" >> (Goto p <$> L.identifier) <* sem)
                               <|> (L.keyword "while" >> (WhileStmt p <$> L.parens expression <*> statement))
                               <|> (L.keyword "if" >> (IfStmt p <$> L.parens expression <*> statement <*> optional elseParser))
                               <|> compoundStatement
                               <|> try (ExpressionStmt <$> optional expression <* sem)
                               <|> (LabeledStmt p <$> L.identifier <* L.punctuator ":" <*> statement)
  where
    sem = L.punctuator ";"
    elseParser = L.keyword "else" >> statement

parseEither :: Parser m a -> Parser m b -> Parser m (Either a b)
parseEither pa pb = try (Left <$> pa) <|> Right <$> pb

compoundStatement :: Parser m (Stmt SynAnn)
compoundStatement = do
  p <- getPosition
  x <- L.braces $ many (parseEither declaration statement)
  return $ CompoundStmt p x

--------------------------------------------------------------------------------
-- Declaration Parsers
--------------------------------------------------------------------------------
typeSpecifier :: Parser m (Type SynAnn)
typeSpecifier =     (L.keyword "void"    >> return Void)
                <|> (L.keyword "char"    >> return Char)
                <|> (L.keyword "int"     >> return Int)
                <|> (L.keyword "struct"  >> structSpecifier)

structSpecifier :: Parser m (Type SynAnn)
structSpecifier = try structInline <|> structIdentifier
  where
    structIdentifier = StructIdentifier <$> L.identifier
    structInline = StructInline <$> optional L.identifier <*> L.braces structDeclarationList
    structDeclarationList = many structDeclaration

structDeclaration :: Parser m (StructDeclaration SynAnn)
structDeclaration = StructDeclaration <$> getPosition <*> typeSpecifier <*> L.commaSep declarator <* L.punctuator ";"

-- | like a direct declarator but with many * in front
declarator :: Parser m (Declarator SynAnn)
declarator = do
  p <- getPosition
  pts <- length <$> many (L.punctuator "*")
  if pts > 0 then
      IndirectDeclarator p pts <$> directDeclarator
    else
      directDeclarator


directDeclarator :: Parser m (Declarator SynAnn)
directDeclarator = chainl1unary dcore dparams
  where
    dcore =   L.parens declarator
              <|> (DeclaratorId <$> getPosition <*> L.identifier)
    dparams = getPosition >>= \p -> flip (FunctionDeclarator p) <$> parameterList

abstractDeclarator :: Parser m (AbstractDeclarator SynAnn)
abstractDeclarator =  do
  pts <- length <$> many (L.punctuator "*")
  if pts > 0 then
    IndirectAbstractDeclarator pts <$> directAbstractDeclarator
  else
    directAbstractDeclarator

directAbstractDeclarator :: Parser m (AbstractDeclarator SynAnn)
directAbstractDeclarator = chainl1unary core ops
  where
    core = L.parens abstractDeclarator
    ops = flip AbstractFunctionDeclarator <$> parameterList <|>
          L.brackets (L.punctuator "*" >> return ArrayStar)

declaration :: Parser m (Declaration SynAnn)
declaration = Declaration <$> getPosition <*> typeSpecifier  <*>  initDeclaratorList <* L.punctuator ";"
  where initDeclaratorList = L.commaSep initDeclarator

initDeclarator :: Parser m (InitDeclarator SynAnn)
initDeclarator = do
  d <- declarator
  ini <- optional $ L.punctuator "=" >> initializer -- this is simplified (not spec)
  return $ InitializedDec d ini

initializer :: Parser m (Initializer SynAnn)
initializer = InitializerList <$> initList <|>
              InitializerAssignment <$> assignmentExpr
  where
    initList = L.braces (L.commaSep initializer <* optional L.comma)


parameterList :: Parser m [Parameter SynAnn]
parameterList = L.parens $ L.commaSep parameterDeclaration

parameterDeclaration :: Parser m (Parameter SynAnn)
parameterDeclaration = try x <|>  y
  where x = Parameter <$> typeSpecifier <*> declarator
        y = AbstractParameter <$> typeSpecifier <*> optional abstractDeclarator


identifier :: Parser m (Expr SynAnn)
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
