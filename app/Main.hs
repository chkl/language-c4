{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           System.Environment
import           System.Exit           (exitFailure, exitSuccess)

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C8
import           Data.List             (intercalate)
import           Data.Monoid           ((<>))
import           Data.Word             (Word8)
import           Text.Megaparsec.Error
import           Text.Megaparsec.Pos

import           Lexer


--------------------------------------------------------------------------------
-- TODO: This should probably go into its own file
-- TODO Use `mappend` with ByteStrings is also not very efficient
-- TODO Consider usage of *lazy* bytestrings
class PrettyPrint a where
  prettyPrint :: a -> BS.ByteString

instance PrettyPrint SourcePos where
  prettyPrint s = C8.pack $ intercalate ":" [sourceName s
                                            , show $ unPos $ sourceLine s
                                            , show $ unPos $ sourceColumn s]

instance PrettyPrint CToken where
  prettyPrint (Keyword k)      = "keyword " <> k
  prettyPrint (Identifier i)   = "identifier " <> i
  prettyPrint (DecConstant n)  = "integer-constant "  <> C8.pack (show n)
  prettyPrint (CharConstant c) = "character-constant '" <> c <> "'"
  prettyPrint (StringLit s)    = "string-literal \"" <> s <> "\""
  prettyPrint (Punctuator s)   = "punctuator " <> s

-- TODO: implement "print it out as you read it", so if there was a character
-- "\eof" then print that character and not "\EOF", so don't use `show`
instance PrettyPrint Word8 where
  prettyPrint b = let n = fromEnum b :: Int
                      c = toEnum n :: Char
                      s = show c
                  in C8.pack s

instance PrettyPrint [(CToken, SourcePos)] where
  prettyPrint = C8.unlines . map prettyPrint'
    where prettyPrint' (t,p) = prettyPrint p <> ": " <> prettyPrint t



tokenize :: String -> IO ()
tokenize filename = do
  contents <- BS.readFile filename
  case runLexer filename contents of
    Left err -> do
      putStr $ parseErrorPretty err
      exitFailure
    Right tokens -> do
      BS.putStr $ prettyPrint tokens
      exitSuccess

showHelp :: IO ()
showHelp = C8.putStrLn "Available options: --tokenize and --help"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--tokenize", filename] -> tokenize filename
    _                        -> showHelp
