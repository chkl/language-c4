{-# LANGUAGE OverloadedStrings #-}

module PrettyPrint where

import qualified Data.ByteString.Lazy       as BS
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.List             (intercalate)
import           Data.Monoid           ((<>))
import           Data.Word             (Word8)
import           Text.Megaparsec.Error hiding (ParseError)
import           Text.Megaparsec.Pos

import Types

class PrettyPrint a where
  prettyPrint :: a -> BS.ByteString

-- instance PrettyPrint SourcePos where
--   prettyPrint s = C8.pack $ intercalate ":" [sourceName s
--                                             , show $ unPos $ sourceLine s
--                                             , show $ unPos $ sourceColumn s]

-- instance PrettyPrint CToken where
--   prettyPrint (Keyword k)      = "keyword " <> k
--   prettyPrint (Identifier i)   = "identifier " <> i
--   prettyPrint (DecConstant n)  = "constant "  <> C8.pack (show n)
--   prettyPrint (CharConstant c) = "constant '" <> c <> "'"
--   prettyPrint (StringLit s)    = "string-literal \"" <> s <> "\""
--   prettyPrint (Punctuator s)   = "punctuator " <> s

instance PrettyPrint Word8 where
  prettyPrint b = let n = fromEnum b :: Int
                      c = toEnum n :: Char
                      s = show c
                  in C8.pack s

myParseErrorPretty :: ParseError -> String
myParseErrorPretty e = sourcePosStackPretty (errorPos e) <>
                       ": error: " <>
                       parseErrorTextPretty e
