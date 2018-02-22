{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString    as BS
import           Data.Monoid        ((<>))
import qualified Data.Text.Lazy.IO  as T
import           System.Environment 
import           System.FilePath    (replaceExtension)
import           System.Exit        (exitFailure)
import           System.IO          (hPutStr, stderr, stdout)

import           Language.C4
import Language.C4.Ast (undecorate)


main :: IO ()
main = do
  x <- getArgs
  case x of
        ["--parse", fn]     -> cmdParse fn
        ["--print-ast", fn] -> cmdPrint fn
        ["--debug-ast", fn] -> cmdDebugAst fn
        ["--tokenize", fn]  -> cmdTokenize fn
        ["--compile", fn]   -> cmdCompile fn
        [fn]                -> cmdCompile fn
        _                   -> showHelp

-- | takes a filepath and invokes the parser, semantic analysis and the pretty printer.
cmdPrint :: FilePath -> IO ()
cmdPrint fn = do
  s <- BS.readFile fn
  ast <- runC4IO $ parse fn s >>= analyse
  hPutPrettyPrint ast stdout

-- | takes a filepath and shows an undecorated ast.
cmdDebugAst :: FilePath -> IO ()
cmdDebugAst fn = do
  s <- BS.readFile fn
  ast <- runC4IO $ parse fn s >>= analyse
  Prelude.print (undecorate ast)

-- | takes a filepath and invokes the parser and semantic analysis.
cmdParse :: FilePath -> IO ()
cmdParse fn = do
  s <- BS.readFile fn
  _ <- runC4IO $ parse fn s >>= analyse
  return ()

-- | takes a filepath and invokes the lexer.
cmdTokenize :: FilePath -> IO ()
cmdTokenize fn = do
  s <- BS.readFile fn
  cmd <- tokenize fn s
  runC4IO cmd

-- | takes a filepath and invokes the parser, semantic analysis and the compiler.
cmdCompile :: FilePath -> IO ()
cmdCompile fn = do
  s <- BS.readFile fn
  m <- runC4IO $ parse fn s >>= analyse >>= compile
  T.writeFile (replaceExtension fn "ll") (ppllvm m)

-- | shows the different options of c4.
showHelp :: IO ()
showHelp = putStrLn "Available options: --tokenize --parse --print-ast --compile  and --help"

-- | Runs the c4 monad. If that produces an error, exit with exitFailure,
-- otherwise return the result.
runC4IO :: C4 a -> IO a
runC4IO cmd = case runC4 cmd of
  Left (p, msg)-> do
    BS.hPutStr stderr $ prettyPrintPos p <> ": error: "
    hPutStr stderr msg
    hPutStr stderr "\n"
    exitFailure
  Right y -> return y

