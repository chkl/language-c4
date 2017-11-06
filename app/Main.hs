module Main where

import qualified Data.ByteString.Lazy as BS
import           System.Environment
import           System.Exit          (exitFailure, exitSuccess)
import           System.IO            (hPutStr, stderr)

import           Lexer
import           PrettyPrint


tokenize :: String -> IO ()
tokenize filename = do
  contents <- BS.readFile filename
  result <- runLexer_ filename contents
  case result of
    Left err -> do
      hPutStr stderr $ myParseErrorPretty err
      exitFailure
    Right () -> exitSuccess

showHelp :: IO ()
showHelp = putStrLn "Available options: --tokenize and --help"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--tokenize", filename] -> tokenize filename
    _                        -> showHelp
