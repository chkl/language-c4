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


main :: IO ()
main = do
  x <- getArgs
  case x of
        ["--parse", fn]     -> cmdParse fn
        ["--print-ast", fn] -> cmdPrint fn
        ["--tokenize", fn]  -> cmdTokenize fn
        ["--compile", fn]   -> cmdCompile fn
        [fn]                -> cmdCompile fn
        _                   -> showHelp

cmdPrint :: FilePath -> IO ()
cmdPrint fn = do
  s <- BS.readFile fn
  ast <- runC4IO $ parse fn s >>= analyse
  hPutPrettyPrint ast stdout


cmdParse :: FilePath -> IO ()
cmdParse fn = do
  s <- BS.readFile fn
  _ <- runC4IO $ parse fn s >>= analyse
  return ()

cmdTokenize :: FilePath -> IO ()
cmdTokenize fn = do
  s <- BS.readFile fn
  cmd <- tokenize fn s
  runC4IO cmd

cmdCompile :: FilePath -> IO ()
cmdCompile fn = do
  s <- BS.readFile fn
  m <- runC4IO $ parse fn s >>= analyse >>= compile
  T.writeFile (replaceExtension fn "ll") (ppllvm m)

showHelp :: IO ()
showHelp = putStrLn "Available options: --tokenize --parse --print-ast  and --help"

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

