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



type FieldIdentifier = ByteString
type TypeName = ByteString

-- this is an AST, therefore we give it 'semantic' names.
data Expr = IdentifierExpr  ByteString
                 | Constant  ByteString -- Or Int?
                 | String ByteString
                 | ArrayAccess ByteString Expr
                 | FunctionCall (Maybe ArgumentExpressionList)
                 | FieldAccess ByteString FieldIdentifier
                 | PointerAccess ByteString FieldIdentifier
-- we don't have to handle postfix ops
--                 | PostIncrement Expr
--                 | PostDecrement Expr
--                 | PreIncrement Expr
--                 | PreDecrement Expr
                 | Plus Expr Expr
                 | MinusBinary Expr Expr
                 | MinusUnary Expr
                 | SmallerThan Expr
                 | Equal Expr Expr
                 | NotEqal Expr Expr
                 | BooleanNot Expr
                 | BooleanAnd Expr Expr
                 | BooleanOr Expr Expr
                 | InlineIf Expr Expr Expr
                 | Assignment Expr Expr
                 | SizeOf (Either Expr TypeName)
                 | DerefOp Expr
                 | AddressOp Expr

-- not sure about this
data ArgumentExpressionList  = ArgumentExpressionList [AssignmentExpression]

data AssignmentExpression = Expr
