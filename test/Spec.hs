{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

import qualified Data.ByteString.Lazy as BS
import           System.Environment
import           System.Exit          (exitSuccess)
import           Test.Hspec
import           Test.QuickCheck

import           SpecQC

import           Spec.Lexer           as SL
import           Spec.Parser          as SP

main :: IO ()
main = do
  args <- getArgs
  if "--sample" `elem` args then do
    generateSampleFile
    exitSuccess
  else runTests

-- | a simple IO action that generates a sample file and outputs it to stdout
generateSampleFile :: IO ()
generateSampleFile = do
  samples <- sample' genCFile
  BS.putStr (fst $ head samples)

-- | run both, quickcheck and unit test based tests
runTests :: IO ()
runTests = hspec $ do
    unitTestsLexer
    unitTestsParser
    qcBasedTests
