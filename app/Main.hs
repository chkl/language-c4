module Main where

import qualified Data.ByteString.Lazy as BS
import           System.Environment
import           System.Exit          (exitFailure, exitSuccess)
import           System.IO            (hPutStr, stderr)

import           Lexer
import           Parser               (runParser)
import           PrettyPrint



main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--tokenize", filename] -> tokenize filename
    ["--parse", filename]    -> parse filename
    _                        -> showHelp

tokenize :: String -> IO ()
tokenize filename = do
  contents <- BS.readFile filename
  result <- runLexer_ filename contents
  case result of
    Left err -> do
      hPutStr stderr $ myParseErrorPretty err
      exitFailure
    Right () -> exitSuccess

parse :: String -> IO ()
parse filename = do
  contents <- BS.readFile filename
  case runParser filename contents of
    Left err -> do
      hPutStr stderr $ myParseErrorPretty err
      exitFailure
    Right r -> print r >> exitSuccess

showHelp :: IO ()
showHelp = putStrLn "Available options: --tokenize and --help"

