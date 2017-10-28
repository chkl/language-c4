{-# LANGUAGE FlexibleInstances #-}
module Main where

import           System.Environment
import           System.Exit        (exitFailure, exitSuccess)

import           Data.List          (intercalate)
import           Text.Parsec.Pos
import           Text.Parsec.String

import           Lexer


--------------------------------------------------------------------------------
-- TODO: This should probably go into its own file
class PrettyPrint a where
  prettyPrint :: a -> String

instance PrettyPrint SourcePos where
  prettyPrint s = intercalate ":" [sourceName s, show $ sourceLine s, show $ sourceColumn s]

instance PrettyPrint Token where
  prettyPrint (Keyword k)      = "keyword " ++ k
  prettyPrint (Identifier i)   = "identifier " ++ i
  prettyPrint (DecConstant n)  = "constant "  ++ show n
  prettyPrint (CharConstant c) = "??" -- TODO how is this called?
  prettyPrint (StringLit s)    = "string-literal " ++ show s
  prettyPrint (Punctuator s)   = "punctuator " ++ s


instance PrettyPrint [(Token, SourcePos)] where
  prettyPrint = unlines . map prettyPrint'
    where prettyPrint' (t,p) = prettyPrint p ++ " " ++ prettyPrint t
--------------------------------------------------------------------------------

tokenize :: String -> IO ()
tokenize filename = do
  res <- parseFromFile lexer filename
  case res of
    Left err     -> do print err
                       exitFailure
    Right tokens -> do putStr $ prettyPrint tokens
                       exitSuccess

showHelp :: IO ()
showHelp = print "Available options: --tokenize and --help"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--tokenize", filename] -> tokenize filename
    _                        -> showHelp
