{-# LANGUAGE OverloadedStrings #-}
import           Test.Hspec

import           Control.Monad
import           Lexer
import           Test.QuickCheck
import           Text.Megaparsec.Pos

import           CLangDef

runLexer' = runLexer "test.c"

runLexer_ = fmap (map fst) . runLexer'

newPos' :: Int -> Int -> SourcePos
newPos' l c = SourcePos "test.c" (mkPos l) (mkPos c)

isLeft :: Either a b -> Bool
isLeft  = either (const True) (const False)

isRight :: Either a b -> Bool
isRight = either (const False) (const True)

main :: IO ()
main = hspec $
  describe "lexing" $ do
    it "should parse a single operator '+'" $ do
      runLexer' "+"     `shouldBe` Right [(Punctuator "+", newPos' 1 1)]
      runLexer' " +"    `shouldBe` Right [(Punctuator "+", newPos' 1 2)]
      runLexer' "  +"   `shouldBe` Right [(Punctuator "+", newPos' 1 3)]

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
      runLexer_ "  5 23 -1" `shouldBe` Right [DecConstant 5, DecConstant 23, DecConstant (-1)]

    it "should correctly handle character constants" $ do
      runLexer_ "'c'"   `shouldBe` Right [CharConstant $ w 'c' ]
      runLexer_ "'\'"   `shouldSatisfy` isLeft
      runLexer_ "'\\"   `shouldSatisfy` isLeft
      runLexer_ "'\\x"  `shouldSatisfy` isLeft
      runLexer_ "'\n'"  `shouldSatisfy` isLeft
      runLexer_ "'\\n'"  `shouldBe` Right [CharConstant $ w '\n']

    it "integers and identifiers and chars" $ do
      runLexer_ "'c' 3 identf" `shouldBe` Right [CharConstant (w 'c'), DecConstant 3, Identifier "identf"]

    it "simple string literals" $ do
      runLexer_ " \"blah\""          `shouldBe` Right [StringLit "blah"]
      runLexer_ "\"foo\""            `shouldBe` Right [StringLit "foo"]
      runLexer_ " \"foo\"3"          `shouldBe` Right [StringLit "foo", DecConstant 3]
      runLexer_ " \"foo\" \"bar\""   `shouldBe` Right [StringLit "foo", StringLit "bar"]
      runLexer_ "u8\"test\""         `shouldBe` Right [StringLit "test"]

    it "ex01 should be parsed correctly" $
        runLexer_ "42  if\n    \"bla\\n\"x+" `shouldBe` Right [ DecConstant 42
                                                              , Keyword "if"
                                                              , StringLit "bla\n"
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
