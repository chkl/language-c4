{-# LANGUAGE OverloadedStrings #-}

module Spec.Parser (
  unitTestsParser
                   ) where
import           Data.ByteString.Lazy (ByteString)
import           Data.Monoid          ((<>))
import           Test.Hspec

import           Parser
import           Types

import           Spec.Helper

unitTestsParser :: SpecWith ()
unitTestsParser = do
  testTypeSpecifier
  testExpressions
  testStatements
  testTranslationUnit
  testAbstractDeclarator
  testAbstractDeclarations
  testDirectDeclarations

testTypeSpecifier :: SpecWith ()
testTypeSpecifier = describe "typeSpecifier parser" $ do
    it "parse primitive types correctly" $ do
      testParser typeSpecifier "void" `shouldBe` Right Void
      testParser typeSpecifier "char" `shouldBe` Right Char
      testParser typeSpecifier "int" `shouldBe` Right Int

    it "parse simple struct definition" $ do
      testParser typeSpecifier "struct A" `shouldBe` Right (StructIdentifier "A")
      testParser typeSpecifier "struct A {}" `shouldBe` Right (StructInline (Just "A") [])
      testParser typeSpecifier "struct A {int foo;}" `shouldBe`
        Right (StructInline (Just "A") [StructDeclaration Int [Declarator 0 $ DirectDeclaratorId "foo" ]])


    it "parses declarators" $ do
      testParser declarator "x" `shouldBe` Right  (Declarator 0 $ DirectDeclaratorId "x" )
      testParser declarator "*x" `shouldBe` Right  (Declarator 1 $ DirectDeclaratorId "x" )
      testParser declarator "**x" `shouldBe` Right  (Declarator 2 $ DirectDeclaratorId "x" )
      testParser declarator "f()" `shouldSatisfy` isRight
      testParser declarator "f(int x)" `shouldSatisfy` isRight
      testParser declarator "f(int x, int y)" `shouldSatisfy` isRight

    it "parses struct declarations" $ do
      testParser structDeclaration "int;" `shouldBe`
        Right  (StructDeclaration Int [])

      testParser structDeclaration "int x;" `shouldBe`
        Right  (StructDeclaration Int [ Declarator 0 (DirectDeclaratorId "x" )])

      testParser structDeclaration "int x,y;" `shouldBe`
        Right  (StructDeclaration Int [ Declarator 0 (DirectDeclaratorId "x" )
                                      , Declarator 0 (DirectDeclaratorId "y" )
                                      ])

      testParser structDeclaration "int x,*y;" `shouldBe`
        Right  (StructDeclaration Int [ Declarator 0 (DirectDeclaratorId "x" )
                                      , Declarator 1 (DirectDeclaratorId "y" )
                                      ])

      testParser structDeclaration "int f(int y);" `shouldSatisfy` isRight

      testParser structDeclaration "int f(int y);" `shouldSatisfy` isRight
    it "parses struct declarations" $
      testParser declaration "struct Point {int x; int y;};"  `shouldSatisfy` isRight

    it "parses struct declarations with initializers" $
      testParser declaration "struct Point {int x; int y;} p1, p2;" `shouldSatisfy` isRight


testExpressions :: SpecWith ()
testExpressions = describe "expression parser" $ do
  it "should parse field and pointer accesses" $ do
      testParser postfixUnaryExpr "x.foo" `shouldBe` Right (FieldAccess (ExprIdent "x") (ExprIdent "foo"))
      testParser postfixUnaryExpr "x->foo" `shouldBe` Right (PointerAccess (ExprIdent "x") (ExprIdent "foo"))
      testParser postfixUnaryExpr "x.y.z.foo" `shouldBe` Right (FieldAccess (FieldAccess
                                                 (FieldAccess (ExprIdent "x") (ExprIdent "y"))
                                                              (ExprIdent "z")) (ExprIdent "foo"))
      testParser postfixUnaryExpr "x.y->z.foo" `shouldBe` Right (FieldAccess (PointerAccess
                                                 (FieldAccess (ExprIdent "x") (ExprIdent "y"))
                                                              (ExprIdent "z")) (ExprIdent "foo"))
  it "should parse functions and arrays" $ do
      testParser postfixUnaryExpr "func(x, y)" `shouldBe` Right (Func (ExprIdent "func") (List [ExprIdent "x",ExprIdent "y"]))
      testParser postfixUnaryExpr "myArray[5]" `shouldBe` Right (Array (ExprIdent "myArray") (Constant "5"))
  it "should parse unary operators" $ do
      testParser unaryExpr "&foo" `shouldBe` Right (UExpr Address (ExprIdent "foo"))
      testParser unaryExpr "&func(x, y, z)" `shouldBe`  Right (UExpr Address (Func (ExprIdent "func") (List [ExprIdent "x",ExprIdent "y",ExprIdent "z"])))
      testParser unaryExpr "sizeof f(x)" `shouldBe` Right (UExpr SizeOf (Func (ExprIdent "f") (ExprIdent "x")))
      testParser unaryExpr "sizeof(int)" `shouldSatisfy` isRight
      testParser unaryExpr "sizeof sizeof (int)" `shouldSatisfy` isRight
      testParser unaryExpr "sizeof sizeof x" `shouldSatisfy` isRight
      testParser unaryExpr "-x" `shouldBe` Right (UExpr Neg (ExprIdent "x"))
      testParser unaryExpr "*x" `shouldBe` Right (UExpr Deref (ExprIdent "x"))
      testParser unaryExpr "!x" `shouldBe` Right (UExpr Not (ExprIdent "x"))
  it "parses simple binary operators" $ do
      testParser expression "5 + 4" `shouldBe` (Right $ BExpr Plus (Constant "5") (Constant "4"))
      testParser expression "5 + 2 * 4" `shouldBe` (Right $ BExpr Plus (Constant "5") (BExpr Mult (Constant "2") (Constant "4")))
      testParser binaryExpr   "x + y + z" `shouldBe` (Right $ ExprIdent "x" `plus` ExprIdent "y" `plus` ExprIdent "z")
      testParser binaryExpr "x + y * z" `shouldBe` (Right $ ExprIdent "x" `plus` (ExprIdent "y" `mult` ExprIdent "z"))
      testParser binaryExpr "x + y" `shouldBe` (Right $ ExprIdent "x" `plus` ExprIdent "y")
      testParser binaryExpr "x - y" `shouldBe` (Right $ ExprIdent "x" `minus` ExprIdent "y")
      testParser binaryExpr "x * y" `shouldBe` (Right $ ExprIdent "x" `mult` ExprIdent "y")
      testParser binaryExpr "x < y" `shouldBe` (Right $ ExprIdent "x" `lt` ExprIdent "y")
      testParser binaryExpr "x == y" `shouldBe` (Right $ ExprIdent "x" `eq` ExprIdent "y")
      testParser binaryExpr "x != y" `shouldBe` (Right $ ExprIdent "x" `ineq` ExprIdent "y")
      testParser binaryExpr "x && y" `shouldBe` (Right $ ExprIdent "x" `bAnd` ExprIdent "y")
      testParser binaryExpr "x || y" `shouldBe` (Right $ ExprIdent "x" `bOr` ExprIdent "y")
      testParser binaryExpr "x = y" `shouldBe` (Right $ ExprIdent "x" `assign` ExprIdent "y")
  it "parses ternary expressions" $ do
      testParser expression "5 + 4 < 3 ? 0 : 1" `shouldBe` (Right (Ternary (BExpr LessThan (BExpr Plus (Constant "5") (Constant "4")) (Constant "3")) (Constant "0") (Constant "1")))
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

testAbstractDeclarator :: SpecWith ()
testAbstractDeclarator = it "parses abstract declarators" $ do
  testParser directAbstractDeclarator "[*]" `shouldSatisfy` isRight
-- TODO: Probably, we don't have to handle those
--  testParser directAbstractDeclarator "[static x = 5]" `shouldSatisfy` isRight
--  testParser directAbstractDeclarator "[*][static y = 3]" `shouldSatisfy` isRight
--  testParser directAbstractDeclarator "([*])" `shouldSatisfy` isRight
--  testParser directAbstractDeclarator "([*][static xz = 10])[*]" `shouldSatisfy` isRight

testAbstractDeclarations :: SpecWith ()
testAbstractDeclarations = it "parses declarations with abstract parameters" $ do
  testParser declaration "int f;" `shouldSatisfy` isRight
  testParser declaration "int f();" `shouldSatisfy` isRight
  testParser declaration "int f(int x);" `shouldSatisfy` isRight
  testParser declaration "int f(int[*]);" `shouldSatisfy` isRight
--  testParser declaration "int f(int[static x=3]);" `shouldSatisfy` isRight
  testParser declaration "int f(int[*]);" `shouldSatisfy` isRight
  testParser declaration "int f(int[*][*]);" `shouldSatisfy` isRight
  testParser declaration "int f(int [*]);" `shouldSatisfy` isRight
  testParser declaration "int f(int*[*]);" `shouldSatisfy` isRight
  testParser declaration "int f(int**[*]);" `shouldSatisfy` isRight

testDirectDeclarations :: SpecWith()
testDirectDeclarations = it "parses declarations with (direct) parameters" $ do
  testParser declaration "int f;" `shouldSatisfy` isRight
  testParser declaration "int f(int x);" `shouldSatisfy` isRight
  testParser declaration "int f(int x(char));" `shouldSatisfy` isRight

testStatements :: SpecWith ()
testStatements = describe "statement parser" $ do
  it "parses simple statements (break,continue,goto,return)" $ do
      testParser statement "break;" `shouldBe` Right Break
      testParser statement "continue;" `shouldBe` Right Continue
      testParser statement "goto testlabel;" `shouldBe` Right (Goto "testlabel")
      testParser statement "return;" `shouldBe` Right (Return Nothing)
      testParser statement "return 1;" `shouldBe` Right (Return (Just $ Constant "1"))
  it "parses a labelled statement" $ do
    testParser statement "start: x = 0;" `shouldBe` Right (LabeledStmt "start" (ExpressionStmt $ Just $ ExprIdent "x" `assign` Constant "0"))

testTranslationUnit :: SpecWith ()
testTranslationUnit = describe "translation unit parser" $ do
  it "parses a minimal c file" $
    testParser translationUnit (addFunctionCode <> simpleMainCode ) `shouldSatisfy` isRight
  it "rejects some invalid code" $ do
    testParser translationUnit "int (int x) {;}" `shouldSatisfy` isLeft
    testParser translationUnit "int () {;}" `shouldSatisfy` isLeft
--------------------------------------------------------------------------------
-- some sample definitions
simpleMain :: ExternalDeclaration
simpleMain = ExtDeclarationFunction ( FunctionDefinition Int ( Declarator 0 (DirectDeclaratorId "main"))
                                       (CompoundStmt [Right (Return (Just (Constant "1")))]))
simpleMainCode :: ByteString
simpleMainCode = "int main() { return 1;}"

addFunction :: ExternalDeclaration
addFunction = ExtDeclarationFunction ( FunctionDefinition Int (Declarator 0 (DirectDeclaratorId "add")) stmt)
  where
    stmt = CompoundStmt [Right (Return (Just (ExprIdent "x" `plus` ExprIdent "y")))]

addFunctionCode :: ByteString
addFunctionCode  = "int add(int x, int y) { return x + y; }"
