{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Spec.Parser (
  unitTestsParser
                   ) where
import           Data.ByteString        (ByteString)
import           Data.Monoid            ((<>))
import           Test.Hspec

import           Language.C4.Ast.SynAst
import           Language.C4.Parser

import           Spec.Helper

unitTestsParser :: SpecWith ()
unitTestsParser = do
  testTypeSpecifier
  testExpressions
  testStatements
  testTranslationUnit
--  testAbstractDeclarator
  testAbstractDeclarations
  testDirectDeclarations


testTypeSpecifier :: SpecWith ()
testTypeSpecifier = describe "typeSpecifier parser" $ do
    it "parse primitive types correctly" $ do
      undecorate <$> testParser typeSpecifier "void" `shouldBe` Right Void
      undecorate <$> testParser typeSpecifier "char" `shouldBe` Right Char
      undecorate <$> testParser typeSpecifier "int" `shouldBe` Right Int

    it "parse simple struct definition" $ do
      testParser typeSpecifier "struct A" `shouldBe` Right (StructIdentifier "A")
      testParser typeSpecifier "struct A {}" `shouldBe` Right (StructInline (Just "A") [])
      undecorate <$> testParser typeSpecifier "struct A {int foo;}" `shouldBe`
        Right (StructInline (Just "A") [StructDeclarationUD Int [DeclaratorIdUD "foo" ]])


    it "parses declarators" $ do
      undecorate <$> testParser declarator "x" `shouldBe` Right  (DeclaratorIdUD "x" )
      undecorate <$> testParser declarator "*x" `shouldBe` Right  (IndirectDeclaratorUD $ DeclaratorIdUD "x" )
      undecorate <$> testParser declarator "**x" `shouldBe` Right  (IndirectDeclaratorUD $ IndirectDeclaratorUD $ DeclaratorIdUD "x" )
      testParser declarator "f()" `shouldSatisfy` isRight
      testParser declarator "f(int x)" `shouldSatisfy` isRight
      testParser declarator "f(int x, int y)" `shouldSatisfy` isRight

    it "parses struct declarations" $ do
      undecorate <$> testParser structDeclaration "int;" `shouldBe` Right  (StructDeclarationUD Int [])
      undecorate <$> testParser structDeclaration "int x;" `shouldBe` Right  (StructDeclarationUD Int [ DeclaratorIdUD "x"])
      undecorate <$> testParser structDeclaration "int x,y;" `shouldBe` Right  (StructDeclarationUD Int [ DeclaratorIdUD "x", DeclaratorIdUD "y"])
      undecorate <$> testParser structDeclaration "int x,*y;" `shouldBe` Right  (StructDeclarationUD Int [ DeclaratorIdUD "x" , IndirectDeclaratorUD (DeclaratorIdUD "y" )])
      testParser structDeclaration "int f(int y);" `shouldSatisfy` isRight
      testParser structDeclaration "int f(int y);" `shouldSatisfy` isRight

    it "parses struct declarations" $
      testParser declaration "struct Point {int x; int y;};"  `shouldSatisfy` isRight

    it "parses struct declarations with initializers" $
      testParser declaration "struct Point {int x; int y;} p1, p2;" `shouldSatisfy` isRight

    it "parses declarations with initializers" $ do
      testParser declaration "int x = 5;" `shouldSatisfy` isRight
      testParser declaration "int x = f();" `shouldSatisfy` isRight
      testParser declaration "int x = 3 + 5;" `shouldSatisfy` isRight


testExpressions :: SpecWith ()
testExpressions = describe "expression parser" $ do
  it "should parse field and pointer accesses" $ do
      undecorate <$> testParser postfixUnaryExpr "x.foo" `shouldBe` Right (FieldAccessUD (ExprIdentUD "x") (ExprIdentUD "foo"))
      undecorate <$> testParser postfixUnaryExpr "x->foo" `shouldBe` Right (PointerAccessUD (ExprIdentUD "x") (ExprIdentUD "foo"))
      undecorate <$> testParser postfixUnaryExpr "x.y.z.foo" `shouldBe` Right (FieldAccessUD (FieldAccessUD
                                                 (FieldAccessUD (ExprIdentUD "x") (ExprIdentUD "y"))
                                                              (ExprIdentUD "z")) (ExprIdentUD "foo"))
      undecorate <$> testParser postfixUnaryExpr "x.y->z.foo" `shouldBe` Right (FieldAccessUD (PointerAccessUD
                                                 (FieldAccessUD (ExprIdentUD "x") (ExprIdentUD "y"))
                                                              (ExprIdentUD "z")) (ExprIdentUD "foo"))
  it "should parse function calls and array accesses" $ do
      undecorate <$> testParser postfixUnaryExpr "func(x, y)" `shouldBe` Right (FuncUD (ExprIdentUD "func") (List [ExprIdentUD "x",ExprIdentUD "y"]))
      undecorate <$> testParser postfixUnaryExpr "func()" `shouldBe` Right (FuncUD (ExprIdentUD "func") (List []))
      undecorate <$> testParser postfixUnaryExpr "myArray[5]" `shouldBe` Right (ArrayUD (ExprIdentUD "myArray") (ConstantUD "5"))
  it "should parse unary operators" $ do
      undecorate <$> testParser unaryExpr "&foo" `shouldBe` Right (UExprUD Address (ExprIdentUD "foo"))
      undecorate <$> testParser unaryExpr "&func(x, y, z)" `shouldBe`  Right (UExprUD Address (FuncUD (ExprIdentUD "func") (List [ExprIdentUD "x",ExprIdentUD "y",ExprIdentUD "z"])))
      undecorate <$> testParser unaryExpr "sizeof f(x)" `shouldBe` Right (UExprUD SizeOf (FuncUD (ExprIdentUD "f") (ExprIdentUD "x")))
      undecorate <$> testParser unaryExpr "sizeof(int)" `shouldSatisfy` isRight
      undecorate <$> testParser unaryExpr "sizeof sizeof (int)" `shouldSatisfy` isRight
      undecorate <$> testParser unaryExpr "sizeof sizeof x" `shouldSatisfy` isRight
      undecorate <$> testParser unaryExpr "-x" `shouldBe` Right (UExprUD Neg (ExprIdentUD "x"))
      undecorate <$> testParser unaryExpr "*x" `shouldBe` Right (UExprUD Deref (ExprIdentUD "x"))
      undecorate <$> testParser unaryExpr "!x" `shouldBe` Right (UExprUD Not (ExprIdentUD "x"))
  it "parses simple binary operators" $ do
      undecorate <$> testParser expression "5 + 4" `shouldBe` (Right $ BExprUD Plus (ConstantUD "5") (ConstantUD "4"))
      undecorate <$> testParser expression "5 + 2 * 4" `shouldBe` (Right $ BExprUD Plus (ConstantUD "5") (BExprUD Mult (ConstantUD "2") (ConstantUD "4")))
      undecorate <$> testParser binaryExpr   "x + y + z" `shouldBe` (Right $ ExprIdentUD "x" `plus` ExprIdentUD "y" `plus` ExprIdentUD "z")
      undecorate <$> testParser binaryExpr "x + y * z" `shouldBe` (Right $ ExprIdentUD "x" `plus` (ExprIdentUD "y" `mult` ExprIdentUD "z"))
      undecorate <$> testParser binaryExpr "x + y" `shouldBe` (Right $ ExprIdentUD "x" `plus` ExprIdentUD "y")
      undecorate <$> testParser binaryExpr "x - y" `shouldBe` (Right $ ExprIdentUD "x" `minus` ExprIdentUD "y")
      undecorate <$> testParser binaryExpr "x * y" `shouldBe` (Right $ ExprIdentUD "x" `mult` ExprIdentUD "y")
      undecorate <$> testParser binaryExpr "x < y" `shouldBe` (Right $ ExprIdentUD "x" `lt` ExprIdentUD "y")
      undecorate <$> testParser binaryExpr "x == y" `shouldBe` (Right $ ExprIdentUD "x" `eq` ExprIdentUD "y")
      undecorate <$> testParser binaryExpr "x != y" `shouldBe` (Right $ ExprIdentUD "x" `ineq` ExprIdentUD "y")
      undecorate <$> testParser binaryExpr "x && y" `shouldBe` (Right $ ExprIdentUD "x" `bAnd` ExprIdentUD "y")
      undecorate <$> testParser binaryExpr "x || y" `shouldBe` (Right $ ExprIdentUD "x" `bOr` ExprIdentUD "y")
      undecorate <$> testParser binaryExpr "x = y" `shouldBe` (Right $ ExprIdentUD "x" `assign` ExprIdentUD "y")

  it "parses ternary expressions" $ do
      undecorate <$> testParser expression "5 + 4 < 3 ? 0 : 1" `shouldBe` Right (TernaryUD (BExprUD LessThan (BExprUD Plus (ConstantUD "5") (ConstantUD "4")) (ConstantUD "3")) (ConstantUD "0") (ConstantUD "1"))
  it "parses unary expressions" $ do
    testParser expression "x[5]" `shouldSatisfy` isRight
--    testParser expression "x[]" `shouldSatisfy` isRight -- TODO: Should we allow this?
    testParser expression "f(x)" `shouldSatisfy` isRight
    testParser expression "x.y" `shouldSatisfy` isRight
    testParser expression "x->y" `shouldSatisfy` isRight
    testParser expression "sizeof(y)" `shouldSatisfy` isRight
    testParser expression "&y" `shouldSatisfy` isRight
    testParser expression "-y" `shouldSatisfy` isRight
    testParser expression "!x" `shouldSatisfy` isRight
  it "parses deeply nested expressions" $ do
    testParser expression "((((((((((((((((((((x+y))))))))))))))))))))" `shouldSatisfy` isRight
  it "rejects baseless array access" $ do
    testParser expression "[5]" `shouldSatisfy` isLeft

--testAbstractDeclarator :: SpecWith ()
--testAbstractDeclarator = it "parses abstract declarators" $ do
-- TODO: Probably, we don't have to handle those
--  testParser directAbstractDeclarator "[*]" `shouldSatisfy` isRight
--  testParser directAbstractDeclarator "[static x = 5]" `shouldSatisfy` isRight
--  testParser directAbstractDeclarator "[*][static y = 3]" `shouldSatisfy` isRight
--  testParser directAbstractDeclarator "([*])" `shouldSatisfy` isRight
--  testParser directAbstractDeclarator "([*][static xz = 10])[*]" `shouldSatisfy` isRight

testAbstractDeclarations :: SpecWith ()
testAbstractDeclarations = it "parses declarations with abstract parameters" $ do
  testParser declaration "int f;" `shouldSatisfy` isRight
  testParser declaration "int f();" `shouldSatisfy` isRight
  testParser declaration "int f(int x);" `shouldSatisfy` isRight
--  testParser declaration "int f(int[*]);" `shouldSatisfy` isRight
--  testParser declaration "int f(int[static x=3]);" `shouldSatisfy` isRight
--  testParser declaration "int f(int[*]);" `shouldSatisfy` isRight
--  testParser declaration "int f(int[*][*]);" `shouldSatisfy` isRight
--  testParser declaration "int f(int [*]);" `shouldSatisfy` isRight
--  testParser declaration "int f(int*[*]);" `shouldSatisfy` isRight
--  testParser declaration "int f(int**[*]);" `shouldSatisfy` isRight

testDirectDeclarations :: SpecWith()
testDirectDeclarations = it "parses declarations with (direct) parameters" $ do
  testParser declaration "int f;" `shouldSatisfy` isRight
  testParser declaration "int f(int x);" `shouldSatisfy` isRight
  testParser declaration "int f(int x(char));" `shouldSatisfy` isRight

testStatements :: SpecWith ()
testStatements = describe "statement parser" $ do
  it "parses simple statements (break,continue,goto,return)" $ do
      undecorate <$> testParser statement "break;" `shouldBe` Right BreakUD
      undecorate <$> testParser statement "continue;" `shouldBe` Right ContinueUD
      undecorate <$> testParser statement "goto testlabel;" `shouldBe` Right (GotoUD "testlabel")
      undecorate <$> testParser statement "return;" `shouldBe` Right (ReturnUD Nothing)
      undecorate <$> testParser statement "return 1;" `shouldBe` Right (ReturnUD (Just $ ConstantUD "1"))
  it "parses a labelled statement" $ do
    undecorate <$> testParser statement "start: x = 0;" `shouldBe` Right (LabeledStmtUD "start" (ExpressionStmtUD (ExprIdentUD "x" `assign` ConstantUD "0")))

testTranslationUnit :: SpecWith ()
testTranslationUnit = describe "translation unit parser" $ do
  it "parses a minimal c file" $
    testParser translationUnit (addFunctionCode <> simpleMainCode ) `shouldSatisfy` isRight
  it "rejects some invalid code" $ do
    testParser translationUnit "int (int x) {;}" `shouldSatisfy` isLeft
    testParser translationUnit "int () {;}" `shouldSatisfy` isLeft


--------------------------------------------------------------------------------
simpleMainCode :: ByteString
simpleMainCode = "int main() { return 1;}"


addFunctionCode :: ByteString
addFunctionCode  = "int add(int x, int y) { return x + y; }"
