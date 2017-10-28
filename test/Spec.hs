import           Test.Hspec

import           Lexer
import           Text.Parsec.Pos
import           Text.Parsec.Error
import           Text.Parsec.Prim


runLexer :: String -> Either ParseError [(Token, SourcePos)]
runLexer = runParser lexer () "test"

runLexer_ :: String -> Either ParseError [Token]
runLexer_ = fmap (map fst) . runLexer

main :: IO ()
main = hspec $
  describe "lexing" $ do
    it "should parse a single operator '+'" $ do
      runLexer_ "+"     `shouldBe` Right [Punctuator "+"]
      runLexer_ " +"    `shouldBe` Right [Punctuator "+"]

    it "recognizes consecutive integers" $ do
      runLexer_ "1 2 3" `shouldBe` Right [DecConstant 1, DecConstant 2, DecConstant 3]
      runLexer_ "1"     `shouldBe` Right [DecConstant 1]
      runLexer_ ""      `shouldBe` Right []
      runLexer_ "5\n23" `shouldBe` Right [DecConstant 5, DecConstant 23]
      runLexer_ "  5 23 -1" `shouldBe` Right [DecConstant 5, DecConstant 23, DecConstant (-1)]

    it "integers and identifiers and chars" $ do
      runLexer_ "'c' 3 identf" `shouldBe` Right [CharConstant 'c', DecConstant 3, Identifier "identf"]

    it "simple string literals" $ do
      runLexer_ " \"blah\""            `shouldBe` Right [StringLit "blah"]
      runLexer_ "\"foo\""            `shouldBe` Right [StringLit "foo"]
      runLexer_ " \"foo\"3"          `shouldBe` Right [StringLit "foo", DecConstant 3]
      runLexer_ " \"foo\" \"bar\""   `shouldBe` Right [StringLit "foo", StringLit "bar"]

    it "ex01 should be parsed correctly" $
        runLexer_ "42  if\n    \"bla\\n\"x+" `shouldBe` Right [DecConstant 42,
                                                            Keyword "if",
                                                            StringLit "bla\n",
                                                            Identifier "x",
                                                            Punctuator "+"]

