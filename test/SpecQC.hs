{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module SpecQC ( prop_genCFile
              , prop_genKeyword
              , prop_genPunctuator
              , prop_genCharConst
              , prop_genDecConst
              , prop_genIdent
              , prop_genStringLit
              , genCFile
              ) where

import           Data.ByteString.Short      (ShortByteString)
import           Data.ByteString.Short      (fromShort, toShort)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import           Data.ByteString.Conversion (ToByteString, toByteString)
import           Data.ByteString.Lazy       (toStrict)
import           Data.Monoid                ((<>))
import           Data.Word                  (Word8)
import           Test.QuickCheck

import           Language.C4.CLangDef
import           Language.C4.Lexer
import           Language.C4.Types

type ExampleGen a            = Gen (ShortByteString, a)

-- | Each generator produces a pair consisting of a string (to parse) as well
-- | as the token that would result if that string were correctly parsed.
genKeyword :: ExampleGen CToken
genKeyword = do
  k <- elements allCKeywords
  return (k, Keyword k)

newtype KeywordG = KeywordG (ShortByteString, CToken)
  deriving Show

instance Arbitrary KeywordG where
  arbitrary = KeywordG <$> genKeyword


genIdent :: ExampleGen CToken
genIdent = do
  i <- makeName `suchThat` (`notElem` allCKeywords)
  return (i, Identifier i)

newtype IdentG = IdentG (ShortByteString, CToken)
  deriving Show

instance Arbitrary IdentG where
  arbitrary = IdentG <$> genIdent

-- | This generates a potential identifier but sometimes might also be a keyword
makeName :: Gen ShortByteString
makeName = do
  tl  <- listOf $ elements $ cNonDigit ++ cDigit
  hd  <- elements cNonDigit
  return $ (toShort (BS.concat $ hd:tl))

genDecConst :: ExampleGen CToken
genDecConst = do
  i <- (arbitrary :: Gen Integer) `suchThat` (>= 0)
  let s = toStrict $ toByteString i
  return ((toShort s), DecConstant s)

newtype DecConstG = DecConstG (ShortByteString, CToken)
  deriving Show

instance Arbitrary DecConstG where
  arbitrary = DecConstG <$> genDecConst

newtype SimpleEscapeSequence = SimpleEscapeSequence ByteString
  deriving ToByteString

instance Arbitrary SimpleEscapeSequence where
  arbitrary = do
    c <- elements $ map fst cSimpleEscapeSequences
    return $ SimpleEscapeSequence $ BS.pack [w '\\', c]

newtype CChar = CChar ByteString
  deriving ToByteString

instance Arbitrary CChar where
  arbitrary = do
    let allowedChar = flip notElem [w '\'', w '\n', w '\\']
    char <- (arbitrary :: Gen Word8) `suchThat` allowedChar
    return $ CChar $ BS.singleton char

genCharConstant :: ExampleGen CToken
genCharConstant = do
  s <- oneof [ toByteString <$> (arbitrary :: Gen CChar)
             , toByteString <$> (arbitrary :: Gen SimpleEscapeSequence) ]
  return (toShort ("'" <> toStrict s <> "'"), CharConstant (toStrict s))

newtype CharConstG = CharConstG (ShortByteString, CToken)
  deriving Show

instance Arbitrary CharConstG where
  arbitrary = CharConstG <$> genCharConstant


-- TODO: Add escape sequences
genStringLit :: ExampleGen CToken
genStringLit = do
  let allowedChar = flip notElem [w '\"', w '\n', w '\\']
  s <- listOf1 $ (arbitrary :: Gen Word8) `suchThat` allowedChar
  let str = BS.pack s
  return (toShort ("\"" <> str <> "\""), StringLit str)

newtype StringLitG = StringLitG (ShortByteString, CToken)
  deriving Show

instance Arbitrary StringLitG where
  arbitrary = StringLitG <$> genStringLit


genPunctuator :: ExampleGen CToken
genPunctuator = do
  p <- elements allCPunctuators
  return (toShort p, Punctuator p)

newtype PunctuatorG = PunctuatorG (ShortByteString, CToken)
  deriving Show

instance Arbitrary PunctuatorG where
  arbitrary = PunctuatorG <$> genPunctuator

genWhitespace :: Gen ByteString
genWhitespace =  do
  ws <-  resize 5 $ listOf1 $ frequency [ (10, return " ")
                                       , (1, elements cWhitespace)
                                       ]
  return $ BS.concat ws

notInfixOf :: ByteString -> [Word8]-> Bool
notInfixOf xs cs = not $ BS.isInfixOf xs (BS.pack cs)

genCommentBlock :: Gen ByteString
genCommentBlock = do
  chars  <- listOf (arbitrary :: Gen Word8) `suchThat` notInfixOf "*/"
  let comment = BS.pack chars
  return $ "/*" `BS.append` comment `BS.append` "*/"


genCommentInline :: Gen ByteString
genCommentInline = do
  chars  <- listOf (arbitrary :: Gen Word8) `suchThat` notInfixOf "\n"
  let comment = BS.pack chars
  return $ "//" `BS.append` comment `BS.append` "\n"

genComment :: Gen ByteString
genComment = oneof [genCommentBlock, genCommentInline]

-- | CTokens are generated a little rigidly (and not comprehensively).
-- | The structure is token - whitespace - maybe a comment - more whitespace.
-- | Probably can be improved.
genCToken :: ExampleGen CToken
genCToken = do
  (s, t) <-  oneof [ genKeyword, genIdent, genDecConst
                   , genCharConstant, genStringLit
                   , genPunctuator]
  ws1     <- genWhitespace
  comment <- frequency [ (10, return ""), (1, genComment ) ]
  ws2     <- genWhitespace
  return (toShort ((fromShort s) `BS.append` ws1 `BS.append` comment `BS.append` ws2), t)

-- TODO: resize to bigger files, but this way it's easier to debug
genCFile :: ExampleGen [CToken]
genCFile = do
  stPairs <- resize 5 $ listOf1 genCToken
  let (strs, toks) = unzip stPairs
  return (mconcat strs, toks)

newtype CFileG = CFileG (ShortByteString, [CToken])
  deriving Show

instance Arbitrary CFileG where
  arbitrary = CFileG <$> genCFile

runLexer' :: ByteString -> Either ParseError [CToken]
runLexer' inp = map fst <$> runLexer "test.c" inp

runLexerProp :: ByteString -> CToken -> Bool
runLexerProp s t =
  case runLexer' s of
    Left _     -> False
    Right [t'] -> t == t'
    _          -> False

prop_genKeyword :: KeywordG -> Bool
prop_genKeyword (KeywordG (s,t)) = runLexerProp (fromShort s) t

prop_genIdent :: IdentG -> Bool
prop_genIdent (IdentG (s, t)) = runLexerProp (fromShort s) t

prop_genDecConst :: DecConstG -> Bool
prop_genDecConst (DecConstG (s, t)) = runLexerProp (fromShort s) t

prop_genCharConst :: CharConstG -> Bool
prop_genCharConst (CharConstG (s, t)) = runLexerProp (fromShort s) t

prop_genStringLit :: StringLitG -> Bool
prop_genStringLit (StringLitG (s, t)) = runLexerProp (fromShort s) t

prop_genPunctuator :: PunctuatorG -> Bool
prop_genPunctuator (PunctuatorG (s, t)) = runLexerProp (fromShort s) t

prop_genCFile :: CFileG -> Bool
prop_genCFile (CFileG (inp, expected)) =
  case runLexer' (fromShort inp) of
    Left _     -> False
    Right outp -> outp == expected


