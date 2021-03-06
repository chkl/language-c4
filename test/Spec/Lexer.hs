{-# LANGUAGE OverloadedStrings #-}

module Spec.Lexer ( lexerQC
                  , unitTestsLexer
                  ) where


import           Control.Monad         (forM_)
import           Data.ByteString.Short
import           Test.Hspec
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck

import           Language.C4.CLangDef
import           Language.C4.Lexer

import           Spec.Helper
import           SpecQC

lexerQC :: SpecWith ()
lexerQC = describe "QuickCheck properties" $
  modifyMaxSuccess (const 1000) $ do
    it "generated files" $ property prop_genCFile
    it "all keywords"    $ property prop_genKeyword

unitTestsLexer :: SpecWith ()
unitTestsLexer =
  describe "Lexer unit tests" $ do
    it "should parse a single operator '+'" $ do
      runLexer' "+"     `shouldBe` Right [(Punctuator "+", newPos' 1 1)]
      runLexer' " +"    `shouldBe` Right [(Punctuator "+", newPos' 1 2)]
      runLexer' "  +"   `shouldBe` Right [(Punctuator "+", newPos' 1 3)]

    it "correctly parsed signed decimal constants" $ do
      runLexer_ "+"     `shouldBe` Right [Punctuator "+"]
      runLexer_ "+3"     `shouldBe` Right [Punctuator "+", DecConstant 3]
      runLexer_ "-3"     `shouldBe` Right [Punctuator "-", DecConstant 3]
      runLexer_ "- 3"     `shouldBe` Right [Punctuator "-", DecConstant 3]

    it "should parse null as a decimal constant" $
      runLexer_ "0"     `shouldBe` Right [DecConstant 0]

    it "parse x++++++y correctly (6.4p1 example 2)" $
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
        runLexer' "42  if\n\t\"bla\\n\"x+" `shouldBe` Right [ (DecConstant 42, newPos' 1 1)
                                                            , (Keyword "if",  newPos' 1 5)
                                                            , (StringLit "bla\\n", newPos' 2 2)
                                                            , (Identifier "x", newPos' 2 9)
                                                            , (Punctuator "+", newPos' 2 10) ]
    it "should ignore all kinds of comments" $ do
      runLexer_ "42 /* 12 comment */ id" `shouldBe` Right [ DecConstant 42,  Identifier "id" ]
      runLexer_ "13\n// line comment id" `shouldBe` Right [ DecConstant 13 ]
      runLexer_ "\"str\" // endline xx"  `shouldBe` Right [ StringLit "str"]
      runLexer_ "xx\n//test\nyy"         `shouldBe` Right [ Identifier "xx", Identifier "yy"]
      runLexer_ "xx//\\\ntest\nyy"         `shouldBe` Right [ Identifier "xx", Identifier "yy"]

--    it "should handle 'line-breaks' gracefully" $ do
--      runLexer_ "42 \\\n 23" `shouldBe` Right [DecConstant "\42", DecConstant "\23"]
--      runLexer_ "\"foo\\nbar\"" `shouldBe` Right [StringLit "foobar"]

    it "parse keywords correctly" $
      forM_ allCKeywords $ \k ->
        runLexer_ (fromShort k)  `shouldBe` Right [ Keyword k]

    it "lexer/char_constant_empty" $
      runLexer' "''" `shouldSatisfy` isLeft
      --TODO

    it "lexer/comment_with_CR_only" $
      runLexer_ "foo// \x0d test" `shouldBe` Right [Identifier "foo"]

    it "lexer/comment_multi_line" $do
      runLexer_ "/*\n\n*/" `shouldBe` Right []
      runLexer_ "test /*\n\n*/" `shouldBe` Right [Identifier "test"]
        -- TODO
