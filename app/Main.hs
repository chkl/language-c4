module Main where

import           Control.Monad        (when)
import qualified Data.ByteString.Lazy as BS
import           System.Environment
import           System.Exit          (exitFailure, exitSuccess)
import           System.IO            (hPutStr, stderr, stdout)

import           Lexer
import           Parser               (runParser)

import           PrettyPrinter
import           Text.Pretty.Simple   as PS

type PrintAST = Bool


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--tokenize", filename]  -> tokenize filename
    ["--parse", filename]     -> parse False filename
    ["--print-ast", filename] -> parse True filename
    _                         -> showHelp
tokenize :: String -> IO ()
tokenize filename = do
  contents <- BS.readFile filename
  result <- runLexer_ filename contents
  case result of
    Left err -> do
      hPutStr stderr $ myParseErrorPretty err
      exitFailure
    Right () -> exitSuccess

parse :: PrintAST -> String -> IO ()
parse pr filename = do
  contents <- BS.readFile filename
  case runParser filename contents of
    Left err -> do
      hPutStr stderr $ myParseErrorPretty err
      exitFailure
    Right ast -> do
      if pr
        then hPutPrettyPrint ast stdout
        else PS.pPrint ast
      exitSuccess


showHelp :: IO ()
showHelp = putStrLn "Available options: --tokenize --parse --print-ast  and --help"


