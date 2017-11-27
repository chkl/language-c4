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


runPrinter' :: Printer a -> ByteString
runPrinter' p = let (_,env) = runPrinter p defaultPrinterEnv
                in toLazyByteString (builder env)


testPrettyPrinter :: SpecWith ()
testPrettyPrinter = do
  testPrimitives
  testExpressions


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