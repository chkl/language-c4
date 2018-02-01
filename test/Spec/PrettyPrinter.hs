{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Spec.PrettyPrinter where

import           Prelude                 hiding (print)

import           Control.Monad           (forM_)
import qualified Data.ByteString         as SB
import           Data.ByteString.Builder
import           Data.ByteString.Lazy    (ByteString, fromStrict)
import           Data.FileEmbed
import           Data.List               (isSuffixOf)
import           Data.Monoid             ((<>))
import           GHC.Exts                (sortWith)
import           Test.Hspec

import           Language.C4.Ast.SynAst
import qualified Language.C4.Parser                  as P
import           Language.C4.PrettyPrinter
import           Language.C4.Types

import           Spec.Helper

runPrinter' :: Printer a -> ByteString
runPrinter' p = let (_,env) = runPrinter p defaultPrinterEnv
                in toLazyByteString (builder env)


testPrettyPrinter :: SpecWith ()
testPrettyPrinter = do
  testPrimitives
  testExpressions
  testFunctionDefinition
  testDeclarations
  testAB


testPrimitives :: SpecWith ()
testPrimitives =  describe "primitives" $ do
    it "compose printers" $ do
      runPrinter' (print "x" >> print "y") `shouldBe` "xy"
      runPrinter' (print "x" <> print "y") `shouldBe` "xy"
      runPrinter' (print "x" <> newline <> print "y") `shouldBe` "x\ny"
    it "correctly idents printers" $ do
      runPrinter' (indent (printLn "x" >> printLn "y" >> indent (printLn "z"))) `shouldBe` "\tx\n\ty\n\t\tz\n"

x,y,z :: Expr UD
x = ExprIdentUD "x"
y = ExprIdentUD "y"
z = ExprIdentUD "z"

testExpressions :: SpecWith ()
testExpressions = describe "expression pretty-printer" $ do
  it "prints binary expressions" $ do
    runPrinter' (prettyPrint (ExprIdentUD "x" `plus` ExprIdentUD "y")) `shouldBe` "(x + y)"
    runPrinter' (prettyPrint (ExprIdentUD "x" `minus` ExprIdentUD "y")) `shouldBe` "(x - y)"
  it "prints unary expressions" $ do
    runPrinter' (prettyPrint (UExprUD Neg (ExprIdentUD "foo"))) `shouldBe` "(-foo)"
    runPrinter' (prettyPrint (UExprUD Address (ExprIdentUD "foo"))) `shouldBe` "(&foo)"
  it "prints ternary expressions" $ do
    runPrinter' (prettyPrint (TernaryUD x y z)) `shouldBe` "(x ? y : z)"

testFunctionDefinition :: SpecWith ()
testFunctionDefinition = describe "function definition" $ do
 it "prints simple function definitions " $ do
   roundtrip P.functionDefinition "int (dbl(int x))\n{\n\treturn (2 * x);\n}\n"

testDeclarations :: SpecWith ()
testDeclarations = describe "declarations" $ do
  describe "struct declarations" $ do
    it "roundtrips simple struct declaration" $ do
      roundtrip P.structDeclaration "struct S\n{\n\tint x;\n} s;\n"

-- | tests the 'roundtrip' property for all files in assets/test/ast
testAB :: SpecWith ()
testAB= do
  let files = pairFiles $(embedDir "assets/test/ast/")
  describe "A/B tests" $
    forM_ files $ \((fnA, a), (fnB, b)) -> do
      it ("prints " <> fnA <> " to " <> fnB) $ do
        let res = P.parse fnA (fromStrict a)
        case runC4 res of
          Left err  -> expectationFailure (show err)
          Right ast -> toPrettyString ast `shouldBe` fromStrict b

pairFiles :: [(FilePath, SB.ByteString)] -> [((FilePath, SB.ByteString), (FilePath, SB.ByteString))]
pairFiles files =  let
  inFiles  = sortWith fst $ filter (\fn -> "in.c"  `isSuffixOf` fst fn) files
  outFiles = sortWith fst $ filter (\fn -> "out.c" `isSuffixOf` fst fn) files
  in zip inFiles outFiles
