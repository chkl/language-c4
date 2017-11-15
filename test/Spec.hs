{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

import           Control.Monad
import qualified Data.ByteString.Lazy  as BS
import           Data.Functor.Identity
import           Data.Word             (Word8)
import           System.Environment
import           System.Exit
import           Test.Hspec
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Test.QuickCheck
import qualified Text.Megaparsec       as MP
import           Text.Megaparsec.Pos

import           CLangDef
import           Lexer                 hiding (runLexer_)
import           Parser
import           PrettyPrint           (myParseErrorPretty)
import           SpecQC
import           Types

-- some helper functions to write the tests more succinctly
runLexer' :: BS.ByteString -> Either ParseError [(CToken, SourcePos)]
runLexer' = runLexer "test.c"

runLexer_ :: BS.ByteString -> Either ParseError [CToken]
runLexer_ = fmap (map fst) . runLexer'

newPos' :: Int -> Int -> SourcePos
newPos' l c = SourcePos "test.c" (mkPos l) (mkPos c)

isLeft :: Either a b -> Bool
isLeft  = either (const True) (const False)

isRight :: Either a b -> Bool
isRight  = either (const False) (const True)
--------------------------------------------------------------------------------


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


qcBasedTests :: SpecWith ()
qcBasedTests = describe "QuickCheck" $
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
      runLexer_ "+3"     `shouldBe` Right [Punctuator "+", DecConstant "3"]
      runLexer_ "-3"     `shouldBe` Right [Punctuator "-", DecConstant "3"]
      runLexer_ "- 3"     `shouldBe` Right [Punctuator "-", DecConstant "3"]

    it "should parse null as a decimal constant" $
      runLexer_ "0"     `shouldBe` Right [DecConstant "0"]

    it "parse x++++++y correctly (6.4p1 example 2)" $
      runLexer_ "x+++++y" `shouldBe` Right [ Identifier "x"
                                           , Punctuator "++"
                                           , Punctuator "++"
                                           , Punctuator "+"
                                           , Identifier "y" ]

    it "recognizes consecutive integers" $ do
      runLexer_ "1 2 3" `shouldBe` Right [DecConstant "1", DecConstant "2", DecConstant "3"]
      runLexer_ "1"     `shouldBe` Right [DecConstant "1"]
      runLexer_ ""      `shouldBe` Right []
      runLexer_ "5\n23" `shouldBe` Right [DecConstant "5", DecConstant "23"]
      runLexer_ "  5 23 1" `shouldBe` Right [DecConstant "5", DecConstant "23", DecConstant "1"]

    it "should correctly handle character constants" $ do
      runLexer_ "'c'"   `shouldBe` Right [CharConstant "c" ]
      runLexer_ "'\'"   `shouldSatisfy` isLeft
      runLexer_ "'\\"   `shouldSatisfy` isLeft
      runLexer_ "'\\x"  `shouldSatisfy` isLeft
      runLexer_ "'\n'"  `shouldSatisfy` isLeft
      runLexer_ "'\\n'"  `shouldBe` Right [CharConstant "\\n"]

    it "integers and identifiers and chars" $ do
      runLexer_ "'c' 3 identf" `shouldBe` Right [CharConstant "c", DecConstant "3", Identifier "identf"]
      runLexer_ "dogfx" `shouldBe` Right [Identifier "dogfx"]

    it "simple string literals" $ do
      runLexer_ " \"blah\""          `shouldBe` Right [StringLit "blah"]
      runLexer_ "\"foo\""            `shouldBe` Right [StringLit "foo"]
      runLexer_ " \"foo\"3"          `shouldBe` Right [StringLit "foo", DecConstant "3"]
      runLexer_ " \"foo\" \"bar\""   `shouldBe` Right [StringLit "foo", StringLit "bar"]
      runLexer_ "u8\"test\""         `shouldBe` Right [StringLit "test"]

    it "ex01 should be parsed correctly" $
        runLexer' "42  if\n\t\"bla\\n\"x+" `shouldBe` Right [ (DecConstant "42", newPos' 1 1)
                                                            , (Keyword "if",  newPos' 1 5)
                                                            , (StringLit "bla\\n", newPos' 2 2)
                                                            , (Identifier "x", newPos' 2 9)
                                                            , (Punctuator "+", newPos' 2 10) ]
    it "should ignore all kinds of comments" $ do
      runLexer_ "42 /* 12 comment */ id" `shouldBe` Right [ DecConstant "42",  Identifier "id" ]
      runLexer_ "13\n// line comment id" `shouldBe` Right [ DecConstant "13" ]
      runLexer_ "\"str\" // endline xx"  `shouldBe` Right [ StringLit "str"]
      runLexer_ "xx\n//test\nyy"         `shouldBe` Right [ Identifier "xx", Identifier "yy"]
      runLexer_ "xx//\\\ntest\nyy"         `shouldBe` Right [ Identifier "xx", Identifier "yy"]

--    it "should handle 'line-breaks' gracefully" $ do
--      runLexer_ "42 \\\n 23" `shouldBe` Right [DecConstant "\42", DecConstant "\23"]
--      runLexer_ "\"foo\\nbar\"" `shouldBe` Right [StringLit "foobar"]

    it "parse keywords correctly" $
      forM_ allCKeywords $ \k ->
        runLexer_ k `shouldBe` Right [ Keyword k]

    it "lexer/char_constant_empty" $
      runLexer' "''" `shouldSatisfy` isLeft
      --TODO

    it "lexer/comment_with_CR_only" $
      runLexer_ "foo// \x0d test" `shouldBe` Right [Identifier "foo"]

    it "lexer/comment_multi_line" $do
      runLexer_ "/*\n\n*/" `shouldBe` Right []
      runLexer_ "test /*\n\n*/" `shouldBe` Right [Identifier "test"]
        -- TODO


    it "should parse expressions" $ do
      MP.runParser binaryOp "test.c" "x + y + z" `shouldBe` (Right $ ExprIdent "x" `plus` ExprIdent "y" `plus` ExprIdent "z")
      MP.runParser binaryOp "test.c" "x + y * z" `shouldBe` (Right $ ExprIdent "x" `plus` (ExprIdent "y" `mult` ExprIdent "z"))

    it "should parse field access" $ do
      MP.runParser postExpr "test.c" "x.foo" `shouldBe` (Right (FieldAccess (ExprIdent "x") (ExprIdent "foo")))

    it "should parse field access 2" $ do
      MP.runParser postExpr "test.c" "x.y.z.foo" `shouldBe` (Right (FieldAccess (FieldAccess (
                                                             FieldAccess (ExprIdent "x") (ExprIdent "y")) (ExprIdent "z"))
                                                             (ExprIdent "foo")))
-- | like runParser but sets file name to "test.c" and renders the error message
-- into a human-readable format
testParser :: MP.Token s ~ Word8 =>  MP.Parsec ErrorMsg s a -> s -> Either String a
testParser p i = pe $ MP.runParser p "test.c" i -- :: Either ParseError a
  where
    pe :: Either ParseError a -> Either String a
    pe (Left err) = Left $ myParseErrorPretty err
    pe (Right b)  = Right b

unitTestsParser :: SpecWith ()
unitTestsParser =
  describe "`typeSpecifier`" $ do
    it "parse primitive types correctly" $ do
      testParser typeSpecifier "void" `shouldBe` Right Void
      testParser typeSpecifier "char" `shouldBe` Right Char
      testParser typeSpecifier "int" `shouldBe` Right Int

    it "parse simple struct definition" $ do
      testParser typeSpecifier "struct A" `shouldBe` Right (StructIdentifier "A")
      testParser typeSpecifier "struct A {}" `shouldBe` Right (StructInline (Just "A") [])
--      testParser typeSpecifier "struct A {int foo;}" `shouldBe`
--        Right (StructInline (Just "A") [StructDeclaration Int [Declarator 0 $ DirectDeclaratorId "foo" []]])


    it "parses declarators" $ do
      testParser declarator "x" `shouldBe` Right  (Declarator 0 $ DirectDeclaratorId "x" [])
      testParser declarator "*x" `shouldBe` Right  (Declarator 1 $ DirectDeclaratorId "x" [])
      testParser declarator "**x" `shouldBe` Right  (Declarator 2 $ DirectDeclaratorId "x" [])

    it "parses struct declarations" $ do
      testParser structDeclaration "int;" `shouldBe`
        Right  (StructDeclaration Int [])

      testParser structDeclaration "int x;" `shouldBe`
        Right  (StructDeclaration Int [ Declarator 0 (DirectDeclaratorId "x" [])])

      testParser structDeclaration "int x,y;" `shouldBe`
        Right  (StructDeclaration Int [ Declarator 0 (DirectDeclaratorId "x" [])
                                      , Declarator 0 (DirectDeclaratorId "y" [])
                                      ])

      testParser structDeclaration "int x,*y;" `shouldBe`
        Right  (StructDeclaration Int [ Declarator 0 (DirectDeclaratorId "x" [])
                                      , Declarator 1 (DirectDeclaratorId "y" [])
                                      ])

      testParser structDeclaration "int x,*y;" `shouldBe`
        Right  (StructDeclaration Int [ Declarator 0 (DirectDeclaratorId "x" [])
                                      , Declarator 1 (DirectDeclaratorId "y" [])
                                      ])



plus = BExpr Plus
mult = BExpr Mult
