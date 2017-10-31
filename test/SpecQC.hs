{-# LANGUAGE OverloadedStrings #-}

module SpecQC ( prop_genCFile
              , prop_genKeyword
              , genCFile
              ) where

import           Lexer
import           CLangDef
import           Control.Monad                    (liftM, liftM2, mapM, ap)
import           Data.Char                        (digitToInt)
import           Foreign.C.String                 (castCCharToChar)
import           Foreign.C.Types                  (CChar)
import           Data.List                        (isInfixOf)
import           Test.QuickCheck
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import           Data.Monoid                        ((<>))
import           Data.ByteString.Conversion (fromByteString, toByteString)
import Data.ByteString.Lazy (toStrict)
import           Control.Exception.Base     (assert)
import           Data.Word       (Word8)


type ExampleGen a = Gen (ByteString, a)

-- | Each generator produces a pair consisting of a string (to parse) as well
-- | as the token that would result if that string were correctly parsed.
genKeyword :: ExampleGen CToken
genKeyword = do
  k <- elements allCKeywords
  return (k, Keyword k)

genIdent :: ExampleGen CToken
genIdent = do
  tl  <- listOf $ elements $ cNonDigit ++ cDigit
  hd  <- elements cNonDigit
  let i  = BS.concat $ hd:tl
  return (i, Identifier i)


genDecConst :: ExampleGen CToken
genDecConst = do
  i <- arbitrary
  let s = toStrict $ toByteString i
  return (s, DecConstant i)


genCharConstant :: ExampleGen CToken
genCharConstant = do
  let allowedChar = (flip notElem) cDisallowedChar
  char <- (arbitrary :: Gen Word8) `suchThat` allowedChar
  return ("'" <> BS.singleton char <> "'", CharConstant char)


-- TODO: Add escape sequences
genStringLit :: ExampleGen CToken
genStringLit = do
  let allowedChar = (flip notElem) cDisallowedChar
  s <- listOf $ suchThat (arbitrary :: Gen Word8) allowedChar
  let str = BS.pack s
  return ("\"" <> str <> "\"", StringLit str)


genPunctuator :: ExampleGen CToken
genPunctuator = do
  p <- elements allCPunctuators
  return (p, Punctuator p)


genWhitespace :: Gen ByteString
genWhitespace =  do
  let space = cWhitespace!!0  -- this is also shitty code  :)
  ws <-  resize 5 $ listOf1 (frequency[(10, return space)
                                       ,(1, elements cWhitespace)
                                       ])
  return $ BS.concat ws

genCommentBlock :: Gen ByteString
genCommentBlock = do
  let notInfixOf = \xs cs -> not $ BS.isInfixOf xs (BS.pack cs)
  chars  <- suchThat (listOf (arbitrary :: Gen Word8)) $ notInfixOf "*/"
  let comment = BS.pack chars
  return $ "/*" `BS.append` comment `BS.append` "*/"

genCommentInline :: Gen ByteString
genCommentInline = do
  let notInfixOf = \xs cs -> not $ BS.isInfixOf xs (BS.pack cs)
  chars  <- suchThat (listOf (arbitrary :: Gen Word8)) $ notInfixOf "\n"
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
  comment <- frequency[(10, return ""), (1, genComment )]
  ws2     <- genWhitespace
  return (s `BS.append` ws1 `BS.append` comment `BS.append` ws2, t)


-- TODO: resize to bigger files, but this way it's easier to debug
genCFile :: Gen (ByteString, [CToken])
genCFile = do
  stPairs <- resize 5 $ listOf1 genCToken
  let f (accS, accT) (s, t) = (accS `BS.append` s, accT ++ [t])
  let cFile = foldl f ("", []) stPairs
  return cFile

-- | Testing
newtype KeywordG = KeywordG (ByteString, CToken)
  deriving (Show, Eq)

newtype CFileG = CFileG (ByteString, [CToken])
  deriving (Show, Eq)

instance Arbitrary KeywordG where
  arbitrary = liftM KeywordG genKeyword

instance Arbitrary CFileG where
  arbitrary = liftM CFileG genCFile

runLexer' :: ByteString -> Either ParseError [CToken]
runLexer' inp = fmap (map fst) $ runLexer "test.c" inp

prop_genKeyword :: KeywordG -> Bool
prop_genKeyword (KeywordG (s,t)) =
  case runLexer' s of
    Left _ -> False
    Right [t'] -> t == t'
    _ -> False


prop_genCFile :: CFileG -> Bool
prop_genCFile (CFileG (inp, expected)) =
  case runLexer' inp of
    Left _ -> False
    Right outp -> outp == expected


