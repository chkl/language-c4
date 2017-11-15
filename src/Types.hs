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

--------------------------------------------------------------------------------
-- Root / Translation Units
--------------------------------------------------------------------------------

data FunctionDefinition = FunctionDefinition Type Declarator Stmt
                        deriving (Show, Eq)

data ExternalDeclaration = ExtDeclarationFunction FunctionDefinition
                         | ExtDeclarationDeclaration Declaration
                         deriving (Show, Eq)
--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------
data CToken = Keyword ByteString
            | Identifier ByteString
            | DecConstant ByteString
            | CharConstant ByteString
            | StringLit ByteString
            | Punctuator ByteString
           deriving (Show, Eq)

data BOp = Mult | Plus | Minus | LessThan | EqualsEquals
         | NotEqual | LAnd | LOr
         deriving (Show, Eq)

data UOp = SizeOf | Address | Deref | Neg | Not
         deriving (Show, Eq)

type Ident = ByteString


data Expr = List [Expr]
          | Ternary Expr Expr Expr
          | Assign Expr Expr
          | BExpr BOp Expr Expr
          | UExpr UOp Expr
          | Array Expr Expr
          | Func Expr Expr
          | ExprIdent ByteString
          | Constant ByteString
          | FieldAccess Expr Expr
          | PointerAccess Expr Expr
          | StringLiteral ByteString
          deriving (Show, Eq)


--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------
type Pointers = Int

data Declaration = Declaration Type [InitDeclarator]
  deriving (Show, Eq)

data Type = Void
          | Char
          | Int
          | StructIdentifier Ident
          | StructInline (Maybe Ident) [StructDeclaration]
          deriving (Show, Eq)

data StructDeclaration = StructDeclaration Type [Declarator]
  deriving (Show, Eq)


-- | first parameter is the number of stars
data Declarator = Declarator Pointers DirectDeclarator
  deriving (Show, Eq)

-- that each declarator can have a list of parameter list is due to an ambiguous grammar
data DirectDeclarator = DirectDeclaratorId Ident [[Parameter]]
                      | DirectDeclaratorParens Declarator [[Parameter]] -- this may be simplified
--                      | DirectDeclaratorWithParams DirectDeclarator (Maybe [Parameter])
                      deriving (Show, Eq)

-- | In contrast to the spec this takes only one initializer
data InitDeclarator = InitializedDec Declarator (Maybe Initializer)
  deriving (Show, Eq)

data Initializer = InitializerAssignment Expr -- assignment expression
                 | InitializerList [Initializer]
  deriving (Show, Eq)

data Parameter =  Parameter Type Declarator
--               |  ParameterAbstract Type (Maybe AbstractDec)
  deriving (Show, Eq)

data AbstractDec = AbstractDecPointed Pointers AbstractDec
                 | AbstractDecStaticExpr (Maybe AbstractDec) Expr
                 | AbstractDecExpr (Maybe AbstractDec)
                 | AbstractDecParam (Maybe AbstractDec) [Parameter]
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------


data Stmt = LabeledStmt Ident Stmt
          | CompoundStmt [Either Declaration Stmt]
          | ExpressionStmt (Maybe Expr)
          | IfStmt Expr Stmt (Maybe Stmt)
          | WhileStmt Expr Stmt
          | Goto Ident
          | Continue
          | Break
          | Return (Maybe Expr)
  deriving (Show, Eq)



data Associativity = LeftAssoc | RightAssoc

data BOperator m = BOperator { associativity :: Associativity
                             , operatorP     :: BOp
                             , opParser      :: Parser m (Expr -> Expr -> Expr)
                             , precedence    :: Int
                             }
