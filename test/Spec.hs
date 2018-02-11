{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

import qualified Data.ByteString       as BS
import           Data.ByteString.Short
import           System.Environment
import           System.Exit           (exitSuccess)
import           Test.Hspec
import           Test.QuickCheck

import           SpecQC

import           Spec.Analysis         as SA
import           Spec.Lexer            as SL
import           Spec.Parser           as SP
import           Spec.PrettyPrinter    as PP

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
  BS.putStr (fromShort.fst $ head samples)

-- | run both, quickcheck and unit test based tests
runTests :: IO ()
runTests = hspec $ do
  describe "Lexer" $ do
    unitTestsLexer
    lexerQC
  describe "Parser" $ do
    unitTestsParser
  describe "Pretty-Printer" $ do
    testPrettyPrinter
  describe "Semantic Analysis" $ do
    testSemanticSamples
