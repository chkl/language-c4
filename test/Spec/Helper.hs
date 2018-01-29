{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Spec.Helper
  (
    plus
  , mult
  , minus
  , lt
  , eq
  , ineq
  , bOr
  , bAnd
  , assign
  , runLexer'
  , runLexer_
  , testParser
  , newPos'
  , isLeft
  , isRight
  , roundtrip
  ) where

import           Ast.SynAst
import qualified Data.ByteString.Lazy  as BS
import           Data.Functor.Identity
import           Data.Word             (Word8)
import           Lexer                 hiding (runLexer_)
import           PrettyPrinter
import           Test.Hspec
import qualified Text.Megaparsec       as MP
import           Text.Megaparsec.Pos
import           Types


--------------------------------------------------------------------------------
--  some helper functions
--------------------------------------------------------------------------------

plus ::  Expr UD ->  Expr UD ->  Expr UD
plus = BExprUD Plus

mult ::  Expr UD ->  Expr UD -> Expr UD
mult = BExprUD Mult

minus ::  Expr UD ->  Expr UD ->  Expr UD
minus = BExprUD Minus

lt ::  Expr UD ->  Expr UD ->  Expr UD
lt = BExprUD LessThan

eq ::  Expr UD ->  Expr UD ->  Expr UD
eq = BExprUD EqualsEquals

ineq ::  Expr UD ->  Expr UD ->  Expr UD
ineq = BExprUD NotEqual

bOr ::  Expr UD ->  Expr UD ->  Expr UD
bOr = BExprUD LOr

bAnd ::  Expr UD ->  Expr UD ->  Expr UD
bAnd = BExprUD LAnd

assign ::  Expr UD ->  Expr UD ->  Expr UD
assign = BExprUD AssignOp

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

-- | like runParser but sets file name to "test.c" and renders the error message
-- into a human-readable format
testParser :: (MP.Token s ~ Word8, MP.Stream s) =>  MP.Parsec ErrorMsg s a -> s -> Either String a
testParser p i = pe $ MP.runParser (p <* MP.eof) "test.c" i -- :: Either ParseError a
  where
    pe :: Either ParseError a -> Either String a
    pe (Left err) = Left $ myParseErrorPretty err
    pe (Right b)  = Right b




roundtrip :: (PrettyPrint a) => MP.Parsec ErrorMsg BS.ByteString a -> BS.ByteString -> Expectation
roundtrip p s = case testParser p s of
  Left err  -> expectationFailure err
  Right ast -> toPrettyString ast `shouldBe` s
