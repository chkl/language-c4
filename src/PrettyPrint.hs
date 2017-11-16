{-# LANGUAGE OverloadedStrings #-}

module PrettyPrint where

import qualified Data.ByteString.Lazy       as BS
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.Monoid           ((<>))
import           Data.Word             (Word8)
import           Text.Megaparsec.Error hiding (ParseError)

import Types

class PrettyPrint a where
  prettyPrint :: a -> BS.ByteString

instance PrettyPrint Word8 where
  prettyPrint b = let n = fromEnum b :: Int
                      c = toEnum n :: Char
                      s = show c
                  in C8.pack s

myParseErrorPretty :: ParseError -> String
myParseErrorPretty e = sourcePosStackPretty (errorPos e) <>
                       ": error: " <>
                       parseErrorTextPretty e
