{-# LANGUAGE OverloadedStrings #-}

module Spec.Parser (
  unitTestsParser
                   ) where

import           Test.Hspec

import           Parser
import           Types

import           Spec.Helper


unitTestsParser :: SpecWith ()
unitTestsParser = do
  testTypeSpecifier
  testStatements

testTypeSpecifier = describe "`typeSpecifier`" $ do
    it "parse primitive types correctly" $ do
      testParser typeSpecifier "void" `shouldBe` Right Void
      testParser typeSpecifier "char" `shouldBe` Right Char
      testParser typeSpecifier "int" `shouldBe` Right Int

    it "parse simple struct definition" $ do
      testParser typeSpecifier "struct A" `shouldBe` Right (StructIdentifier "A")
      testParser typeSpecifier "struct A {}" `shouldBe` Right (StructInline (Just "A") [])
      testParser typeSpecifier "struct A {int foo;}" `shouldBe`
        Right (StructInline (Just "A") [StructDeclaration Int [Declarator 0 $ DirectDeclaratorId "foo" []]])


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

      testParser structDeclaration "int f(int y);" `shouldBe`
        Right (StructDeclaration Int [ Declarator 0 (
                                         DirectDeclaratorId "f" [
                                             [Parameter Int (Declarator 0 (DirectDeclaratorId "y" []))]]
                                         )
                                     ])

      testParser structDeclaration "int f(int y);" `shouldBe`
        Right (StructDeclaration Int [ Declarator 0 (
                                         DirectDeclaratorId "f" [
                                             [Parameter Int (Declarator 0 (DirectDeclaratorId "y" []))]]
                                         )
                                     ])
    it "parses struct declarations" $
      testParser declaration "struct Point {int x; int y;};" `shouldBe`
        (Right $ Declaration (StructInline (Just "Point")
                              [ StructDeclaration Int [ Declarator 0 $ DirectDeclaratorId "x" [] ]
                              , StructDeclaration Int [ Declarator 0 $ DirectDeclaratorId "y" [] ]
                              ]) [])

    it "parses struct declarations with initializers" $
      testParser declaration "struct Point {int x; int y;} p1, p2;" `shouldBe`
        (Right $ Declaration (StructInline (Just "Point")
                              [ StructDeclaration Int [ Declarator 0 $ DirectDeclaratorId "x" [] ]
                              , StructDeclaration Int [ Declarator 0 $ DirectDeclaratorId "y" [] ]
                              ])
         [ InitializedDec (Declarator 0 (DirectDeclaratorId "p1" [])) Nothing
         , InitializedDec (Declarator 0 (DirectDeclaratorId "p2" [])) Nothing ]
        )



testStatements = describe "statements" $ do
  it "parses a labeled statement" $ do
      testParser statement "break;" `shouldBe` Right Break
