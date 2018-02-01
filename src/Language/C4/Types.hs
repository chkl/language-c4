{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}


module Language.C4.Types
  ( module Language.C4.Types
  ,  SourcePos -- re-export SourcePos
  ) where

import           Control.Monad.Error.Class
import           Control.Monad.Trans.Except
import qualified Data.ByteString.Lazy.Char8 as C8

import           Data.ByteString.Lazy       (ByteString)
import           Data.List                  (intercalate)
import           Data.List.NonEmpty         (head)
import           Data.Word                  (Word8)
import           Prelude                    hiding (head)
import           Text.Megaparsec
import qualified Text.Megaparsec.Error      as E
import           Text.Megaparsec.Pos        (SourcePos)


newtype ErrorMsg = ErrorMsg { toString :: String
                         } deriving (Ord, Eq, Show)

instance ShowErrorComponent ErrorMsg where
  showErrorComponent e = "error: " ++ toString e

type ParseError = E.ParseError Word8 ErrorMsg

instance C4Error Language.C4.Types.ParseError where
  getErrorComponent = parseErrorTextPretty
  getErrorPosition =   head . errorPos



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

class C4Error a where
  getErrorPosition :: a -> SourcePos
  getErrorComponent :: a -> String

-- this will be the main monad in which the big pieces run. (It allows to throw compiler errors)
-- If necessary it will also allow to read configuration and runtime parameters
--type C4 a = (IsC4Error e) => Except e a
newtype C4 a = C4 {unC4 :: Except (SourcePos, String) a }
  deriving (Functor, Applicative, Monad, MonadError (SourcePos, String))

runC4 :: C4 a -> Either (SourcePos, String) a
runC4 = runExcept . unC4

throwC4 :: (C4Error e) => e -> C4 a
throwC4 e = C4 (throwE e')
  where e' = (getErrorPosition e, getErrorComponent e)


prettyPrintPos :: SourcePos -> ByteString
prettyPrintPos s = C8.pack $ intercalate ":" [sourceName s
                                             , show $ unPos $ sourceLine s
                                             , show $ unPos $ sourceColumn s]
