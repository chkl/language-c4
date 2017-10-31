{-# LANGUAGE OverloadedStrings #-}
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
import           Data.ByteString.Conversion (fromByteString)
import           Control.Exception.Base     (assert)
import           Data.Word       (Word8)

-- | Each generator produces a pair consisting of a string (to parse) as well
-- | as the token that would result if that string were correctly parsed.
genKeyword :: Gen (ByteString, CToken)
genKeyword = do
  k <- elements allCKeywords
  return (k, Keyword k)

genIdent :: Gen (ByteString , CToken)
genIdent = do
  tail  <- listOf $ elements $ cNonDigit ++ cDigit
  head  <- elements cNonDigit
  let i  = BS.concat $ head:tail
  return (i, Identifier i)


genDecConst :: Gen (ByteString, CToken)
genDecConst = do
  digits        <- listOf $ elements cDigit
  nonZeroDigit  <- elements cNonZeroDigit
  let decConstBS = BS.concat $ nonZeroDigit:digits
  let dC  = case fromByteString decConstBS :: Maybe Integer of
              Just i -> i
              Nothing -> assert False 1 -- this is shitty code :)
  return (decConstBS, DecConstant dC)
  
genCharConstant :: Gen (ByteString, CToken)
genCharConstant = do
  let allowedChar = not . (flip elem) cDisallowedChar
  char <- suchThat (arbitrary :: Gen Word8) allowedChar
  return (BS.singleton char, CharConstant char)


genStringLit :: Gen (ByteString, CToken)
genStringLit = do 
  let allowedChar = not . (flip elem) cDisallowedChar
  s <- listOf $ suchThat (arbitrary :: Gen Word8) allowedChar
  let str = BS.pack s
  return (str, StringLit str)


genPunctuator :: Gen (ByteString, CToken)
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

-- | Tokens are generated a little rigidly (and not comprehensively).
-- | The structure is token - whitespace - maybe a comment - more whitespace. 
-- | Probably can be improved.
genToken :: Gen (ByteString, CToken)
genToken = do
  (s, t) <-  oneof [ genKeyword, genIdent, genDecConst
                   , genCharConstant, genStringLit
                   , genPunctuator]
  ws1     <- genWhitespace
  comment <- frequency[(10, return ""), (1, genComment )]
  ws2     <- genWhitespace
  return (s `BS.append` ws1 `BS.append` comment `BS.append` ws2, t)

genCFile :: Gen (ByteString, [CToken])
genCFile = do
  stPairs <- resize 50 $ listOf1 genToken
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
  
runLexerNoPos s = case runLexer "test.c" s of
  Left error -> []
  Right xs   -> map fst xs

prop_genKeyword keywordPair = [t] == runLexerNoPos s
 where KeywordG (s, t) = keywordPair
       types = keywordPair :: KeywordG




