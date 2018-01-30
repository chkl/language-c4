module Main where

import qualified Data.ByteString.Lazy as BS
import           System.Environment
import           System.Exit          (exitFailure, exitSuccess)
import           System.IO            (hPutStr, stderr, stdout)

import           Analysis
import           Ast.SemAst
import           Lexer
import           Parser               (runParser)
import           PrettyPrinter


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
        else do
          putStrLn "---------------- AST ----------------"
          -- PS.pPrint ast
          putStrLn "--------- SEMANTIC ANALYSIS ---------"
          putStrLn "semantic analysis"
          case semanticAnalysis ast of
            Left err -> do
              Prelude.putStrLn "errors:"
              mapM_ Prelude.print err
              exitFailure
            Right (TranslationUnit s _) -> do
--              Prelude.print ast'
              putStrLn "top scope:"
              Prelude.print s
              exitSuccess


showHelp :: IO ()
showHelp = putStrLn "Available options: --tokenize --parse --print-ast  and --help"


