
module Types
  ( module Types
  ,  SourcePos -- ^re-export SourcePos
  ) where

import           Data.ByteString.Lazy  (ByteString)
import           Data.Word             (Word8)
import           Text.Megaparsec
import Text.Megaparsec.Pos (SourcePos)
import qualified Text.Megaparsec.Error as E


newtype ErrorMsg = ErrorMsg { toString :: String
                         } deriving (Ord, Eq, Show)

instance ShowErrorComponent ErrorMsg where
  showErrorComponent e = "error: " ++ toString e

type ParseError = E.ParseError Word8 ErrorMsg


type Parser m a = ParsecT ErrorMsg ByteString m a

type Ident = ByteString

data CToken = Keyword ByteString
            | Identifier ByteString
            | DecConstant ByteString
            | CharConstant ByteString
            | StringLit ByteString
            | Punctuator ByteString
           deriving (Show, Eq)


data Associativity = LeftAssoc | RightAssoc

