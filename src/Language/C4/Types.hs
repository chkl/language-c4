{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleContexts #-}


module Language.C4.Types
  ( module Language.C4.Types
  ,  SourcePos -- re-export SourcePos
  ) where

import           Control.Monad.Error.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import qualified Data.ByteString.Char8      as C8
import           Data.Functor.Identity

import           Data.ByteString            (ByteString)
import           Data.List                  (intercalate)
import           Data.List.NonEmpty         (head)
import           Data.Word                  (Word8)
import           Prelude                    hiding (head)
import           Text.Megaparsec
import qualified Text.Megaparsec.Error      as E
import           Text.Megaparsec.Pos        (SourcePos)

-- | This will be the main monad in which the big pieces run. (It allows to throw compiler errors via @throwC4)
-- If necessary it will also allow to read configuration and runtime parameters.
newtype C4T m a = C4T {unC4 :: ExceptT (SourcePos, String) m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadError (SourcePos, String))

runC4T :: C4T m a -> m (Either (SourcePos, String) a)
runC4T = runExceptT . unC4

type C4 = C4T Identity
runC4 :: C4 a -> Either (SourcePos, String) a
runC4 = runIdentity . runC4T

throwC4 :: (C4Error e, MonadError (SourcePos, String) m) => e -> m a
throwC4 e = throwError e'
  where e' = (getErrorPosition e, getErrorComponent e)

-- | A @C4Error is an error that a location and an error message.
class C4Error a where
  getErrorPosition :: a -> SourcePos
  getErrorComponent :: a -> String



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

prettyPrintPos :: SourcePos -> ByteString
prettyPrintPos s = C8.pack $ intercalate ":" [sourceName s
                                             , show $ unPos $ sourceLine s
                                             , show $ unPos $ sourceColumn s]
