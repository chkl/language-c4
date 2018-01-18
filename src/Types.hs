{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeFamilies       #-}

module Types where

import           Data.ByteString.Lazy  (ByteString)
import           Data.Word             (Word8)
import           Text.Megaparsec
import qualified Text.Megaparsec.Error as E

newtype ErrorMsg = ErrorMsg { toString :: String
                         } deriving (Ord, Eq, Show)

instance ShowErrorComponent ErrorMsg where
  showErrorComponent e = "error: " ++ toString e

type ParseError = E.ParseError Word8 ErrorMsg


type Parser m a = ParsecT ErrorMsg ByteString m a

data CToken = Keyword ByteString
            | Identifier ByteString
            | DecConstant ByteString
            | CharConstant ByteString
            | StringLit ByteString
            | Punctuator ByteString
           deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Root / Translation Units
--------------------------------------------------------------------------------
type family AnnTranslationUnit x
type family AnnFunctionDefinition x
type family AnnTernary x
type family AnnAssign x
type family AnnArray x
type family AnnBExpr x
type family AnnUExpr x
type family AnnFunc x
type family AnnSizeOfType x
type family AnnExprIdent x
type family AnnConstant x
type family AnnFieldAccess x
type family AnnPointerAccess x
type family AnnStringLiteral x
type family AnnDeclaration x
type family AnnIndirectDeclarator x
type family AnnStructDeclaration x
type family AnnDeclaratorId x
type family AnnFunctionDeclarator x
type family AnnCompoundStmt x
type family AnnIfStmt x
type family AnnWhileStmt x
type family AnnGoto x
type family AnnContinue x
type family AnnBreak x
type family AnnReturn x
type family AnnLabeledStmt x

data TranslationUnit x = TranslationUnit (AnnTranslationUnit x) [ExternalDeclaration x]


type ExternalDeclaration x = Either (Declaration x) (FunctionDefinition x)

data FunctionDefinition x = FunctionDefinition (AnnFunctionDefinition x) (Type x) (Declarator x) (Stmt x)


--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

data BOp = Mult | Plus | Minus | LessThan | EqualsEquals
         | NotEqual | LAnd | LOr | AssignOp
         deriving (Show, Eq)

data UOp = SizeOf | Address | Deref | Neg | Not
         deriving (Show, Eq)

type Ident = ByteString


data Expr x = List [Expr x]
          | Ternary (AnnTernary x) (Expr x) (Expr x) (Expr x)
          | Assign (AnnAssign x) (Expr x) (Expr x)
          | BExpr (AnnBExpr x) BOp (Expr x) (Expr x)
          | UExpr (AnnUExpr x) UOp (Expr x)
          | SizeOfType (AnnSizeOfType x) (Type x)
          | Array (AnnArray x) (Expr x) (Expr x)
          | Func (AnnFunc x) (Expr x) (Expr x)
          | ExprIdent (AnnExprIdent x) ByteString
          | Constant (AnnConstant x) ByteString
          | FieldAccess (AnnFieldAccess x) (Expr x) (Expr x)
          | PointerAccess (AnnPointerAccess x) (Expr x) (Expr x)
          | StringLiteral (AnnStringLiteral x) ByteString


--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------
type Pointers = Int

data Declaration x = Declaration (AnnDeclaration x) (Type x) [InitDeclarator x]

data Type x = Void
          | Char
          | Int
          | StructIdentifier Ident
          | StructInline (Maybe Ident) [StructDeclaration x]

data StructDeclaration x = StructDeclaration (AnnStructDeclaration x) (Type x) [Declarator x ]


-- | first parameter is the number of stars
data Declarator x = IndirectDeclarator (AnnIndirectDeclarator x) Pointers (Declarator x)
                  | DeclaratorId (AnnDeclaratorId x) Ident
                  | FunctionDeclarator (AnnFunctionDeclarator x) (Declarator x) [Parameter x]

-- TODO: Find a way to unify declarator and abstract declarator
data AbstractDeclarator x = IndirectAbstractDeclarator Pointers (AbstractDeclarator x)
                        | AbstractFunctionDeclarator (AbstractDeclarator x) [Parameter x]
                        | ArrayStar (AbstractDeclarator x)

-- | In contrast to the spec this takes only one initializer
data InitDeclarator x = InitializedDec (Declarator x) (Maybe (Initializer x))


data Initializer x = InitializerAssignment (Expr x) -- assignment expression
                 | InitializerList [Initializer x]

data Parameter x =  Parameter (Type x) (Declarator x)
               |  AbstractParameter (Type x) (Maybe (AbstractDeclarator x))


--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------

data Stmt x = LabeledStmt (AnnLabeledStmt x) Ident (Stmt x)
          | CompoundStmt (AnnCompoundStmt x) [Either (Declaration x) (Stmt x)]
          | ExpressionStmt (Maybe (Expr x))
          | IfStmt (AnnIfStmt x) (Expr x) (Stmt x) (Maybe (Stmt x))
          | WhileStmt (AnnWhileStmt x) (Expr x) (Stmt x)
          | Goto (AnnGoto x) Ident
          | Continue (AnnContinue x)
          | Break (AnnBreak x)
          | Return (AnnReturn x) (Maybe (Expr x))

data Associativity = LeftAssoc | RightAssoc

data BOperator m = BOperator { associativity :: Associativity
                             , operatorP     :: BOp
                             , opParser      :: forall a . Parser m (Expr a -> Expr a -> Expr a)
                             , precedence    :: Int
                             }
