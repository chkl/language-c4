{-# LANGUAGE OverloadedStrings #-}

module Spec.PrettyPrinter where

import           Prelude                 hiding (print)

import           Data.ByteString.Builder
import           Data.ByteString.Lazy    (ByteString)
import           Data.Monoid             ((<>))
import           Test.Hspec

import           PrettyPrinter
import           Spec.Helper
import           Types
import qualified Parser as P


runPrinter' :: Printer a -> ByteString
runPrinter' p = let (_,env) = runPrinter p defaultPrinterEnv
                in toLazyByteString (builder env)


testPrettyPrinter :: SpecWith ()
testPrettyPrinter = do
  testPrimitives
  testExpressions
  testFunctionDefinition
  testDeclarations


testPrimitives :: SpecWith ()
testPrimitives =  describe "primitives" $ do
    it "compose printers" $ do
      runPrinter' (print "x" >> print "y") `shouldBe` "xy"
      runPrinter' (print "x" <> print "y") `shouldBe` "xy"
      runPrinter' (print "x" <> newline <> print "y") `shouldBe` "x\ny"
    it "correctly idents printers" $ do
      runPrinter' (ident (printLn "x" >> printLn "y" >> ident (printLn "z"))) `shouldBe` "\tx\n\ty\n\t\tz\n"

x,y,z :: Expr
x = ExprIdent "x"
y = ExprIdent "y"
z = ExprIdent "z"

testExpressions :: SpecWith ()
testExpressions = describe "expression pretty-printer" $ do
  it "prints binary expressions" $ do
    runPrinter' (prettyPrint (ExprIdent "x" `plus` ExprIdent "y")) `shouldBe` "(x + y)"
    runPrinter' (prettyPrint (ExprIdent "x" `minus` ExprIdent "y")) `shouldBe` "(x - y)"
  it "prints unary expressions" $ do
    runPrinter' (prettyPrint (UExpr Neg (ExprIdent "foo"))) `shouldBe` "-(foo)"
    runPrinter' (prettyPrint (UExpr Address (ExprIdent "foo"))) `shouldBe` "&(foo)"
  it "prints ternary expressions" $ do
    runPrinter' (prettyPrint (Ternary x y z)) `shouldBe` "(x ? y : z)"

testFunctionDefinition :: SpecWith ()
testFunctionDefinition = describe "function definition" $ do
 it "prints simple function definitions " $ do
   roundtrip P.functionDefinition "int dbl(int x)\n{\n\treturn (2 * x);\n}\n"

testDeclarations :: SpecWith ()
testDeclarations = describe "declarations" $ do
  describe "struct declarations" $ do
    it "roundtrips simple struct declaration" $ do
      roundtrip P.structDeclaration "struct S\n{\n\tint x;\n} s;\n"
