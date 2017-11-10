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

type Pointers = Int

data Declaration = Declaration Type (Either Dec InitializedDec)

data Type = Void | Char | Int

-- | first parameter is the number of stars
data Dec = Dec Pointers [Dec] (Maybe [Parameter])

data InitializedDec = InitializedDec Dec [Expr]

data Parameter =  Parameter Type Dec
               |  ParameterAbstract Type (Maybe AbstractDec)

data AbstractDec = AbstractDecPointed Pointers AbstractDec
                 | AbstractDecStaticExpr (Maybe AbstractDec) Expr
                 | AbstractDecExpr (Maybe AbstractDec)
                 | AbstractDecParam (Maybe AbstractDec) [Parameter]


data Stmt = LabeledStmt Ident Stmt
          | CompoundStmt [Either Declaration Stmt]
          | ExpressionStmt (Maybe Expr)
          | IfStmt Expr Stmt (Maybe Stmt)
          | WhileStmt Expr Stmt
          | Goto Ident
          | Continue
          | Break
          | Return (Maybe Expr)



data Associativity = LeftAssoc | RightAssoc

data BOperator m = BOperator { associativity :: Associativity
                             , operatorP     :: BOp
                             , opParser      :: Parser m (Expr -> Expr -> Expr)
                             , precedence    :: Int
                             }
