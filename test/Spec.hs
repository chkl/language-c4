{-# LANGUAGE OverloadedStrings #-}
import           Test.Hspec
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import qualified Data.ByteString          as BS
import           Control.Monad
import           Lexer
import           System.Environment
import           System.Exit
import           Test.QuickCheck
import           Text.Megaparsec.Pos

import           CLangDef

import           SpecQC

runLexer' = runLexer "test.c"

runLexer_ = fmap (map fst) . runLexer'

newPos' :: Int -> Int -> SourcePos
newPos' l c = SourcePos "test.c" (mkPos l) (mkPos c)

isLeft :: Either a b -> Bool
isLeft  = either (const True) (const False)

isRight :: Either a b -> Bool
isRight = either (const False) (const True)


generateSampleFile :: IO ()
generateSampleFile = do
  samples <- sample' genCFile
  BS.putStr (fst $ head samples)

main :: IO ()
main = do
  args <- getArgs
  if ("--sample" `elem` args) then do
    generateSampleFile
    exitSuccess
  else runTests

runTests :: IO ()
runTests = hspec $ do
    qcBasedTests
    unitTests


qcBasedTests = describe "QuickCheck" $ do
  modifyMaxSuccess (const 100000) $ do
    it "generated files" $ property prop_genCFile
    it "all keywords"    $ property prop_genKeyword

unitTests = do
  describe "lexing" $ do
    it "run quickcheck (randomly generated files)" $ do
      property prop_genCFile
    it "should parse a single operator '+'" $ do
      runLexer' "+"     `shouldBe` Right [(Punctuator "+", newPos' 1 1)]
      runLexer' " +"    `shouldBe` Right [(Punctuator "+", newPos' 1 2)]
      runLexer' "  +"   `shouldBe` Right [(Punctuator "+", newPos' 1 3)]

    it "correctly parsed signed decimal constants" $ do
      runLexer_ "+"     `shouldBe` Right [Punctuator "+"]
      runLexer_ "+3"     `shouldBe` Right [Punctuator "+", DecConstant 3]
      runLexer_ "-3"     `shouldBe` Right [Punctuator "-", DecConstant 3]
      runLexer_ "- 3"     `shouldBe` Right [Punctuator "-", DecConstant 3]

    it "parse x++++++y correctly (6.4p1 example 2)" $ do
      runLexer_ "x+++++y" `shouldBe` Right [ Identifier "x"
                                           , Punctuator "++"
                                           , Punctuator "++"
                                           , Punctuator "+"
                                           , Identifier "y" ]

    it "recognizes consecutive integers" $ do
      runLexer_ "1 2 3" `shouldBe` Right [DecConstant 1, DecConstant 2, DecConstant 3]
      runLexer_ "1"     `shouldBe` Right [DecConstant 1]
      runLexer_ ""      `shouldBe` Right []
      runLexer_ "5\n23" `shouldBe` Right [DecConstant 5, DecConstant 23]
      runLexer_ "  5 23 1" `shouldBe` Right [DecConstant 5, DecConstant 23, DecConstant 1]

    it "should correctly handle character constants" $ do
      runLexer_ "'c'"   `shouldBe` Right [CharConstant "c" ]
      runLexer_ "'\'"   `shouldSatisfy` isLeft
      runLexer_ "'\\"   `shouldSatisfy` isLeft
      runLexer_ "'\\x"  `shouldSatisfy` isLeft
      runLexer_ "'\n'"  `shouldSatisfy` isLeft
      runLexer_ "'\\n'"  `shouldBe` Right [CharConstant "\\n"]

    it "integers and identifiers and chars" $ do
      runLexer_ "'c' 3 identf" `shouldBe` Right [CharConstant "c", DecConstant 3, Identifier "identf"]
      runLexer_ "dogfx" `shouldBe` Right [Identifier "dogfx"]

    it "simple string literals" $ do
      runLexer_ " \"blah\""          `shouldBe` Right [StringLit "blah"]
      runLexer_ "\"foo\""            `shouldBe` Right [StringLit "foo"]
      runLexer_ " \"foo\"3"          `shouldBe` Right [StringLit "foo", DecConstant 3]
      runLexer_ " \"foo\" \"bar\""   `shouldBe` Right [StringLit "foo", StringLit "bar"]
      runLexer_ "u8\"test\""         `shouldBe` Right [StringLit "test"]

    it "ex01 should be parsed correctly" $
        runLexer_ "42  if\n    \"bla\\n\"x+" `shouldBe` Right [ DecConstant 42
                                                              , Keyword "if"
                                                              , StringLit "bla\\n"
                                                              , Identifier "x"
                                                              , Punctuator "+" ]
    it "should ignore all kinds of comments" $ do
      runLexer_ "42 /* 12 comment */ id" `shouldBe` Right [ DecConstant 42,  Identifier "id" ]
      runLexer_ "13\n// line comment id" `shouldBe` Right [ DecConstant 13 ]
      runLexer_ "\"str\" // endline xx"  `shouldBe` Right [ StringLit "str"]
      runLexer_ "xx\n//test\nyy"         `shouldBe` Right [ Identifier "xx", Identifier "yy"]

    it "parse keywords correctly" $ do
      forM_ allCKeywords $ \k -> do
        runLexer_ k `shouldBe` Right [ Keyword k]

