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

type BOp = ByteString
type UOp = ByteString
type Ident = ByteString

data Expr = Comma Expr Expr
          | Assign Expr Expr
          | BExpr BOp Expr Expr
          | UExpr UOp Expr
          | Array Ident [Expr]
          | Func Ident [Expr]
          | FieldAccess Expr Ident
          | PointerAccess Expr Ident
          | ExprIdentifier ByteString
          | Constant ByteString
          | StringLiteral ByteString


