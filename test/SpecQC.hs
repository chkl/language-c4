import           Lexer
import           CLangDef
import           Control.Monad                    (liftM, liftM2, mapM, ap)
import           Data.Char                        (digitToInt)
import           Foreign.C.String                 (castCCharToChar)
import           Foreign.C.Types                  (CChar)
import           Data.List                        (isInfixOf)
import           Test.QuickCheck

-- | Each generator produces a pair consisting of a string (to parse) as well
-- | as the token that would result if that string were correctly parsed.
genKeyword :: Gen (String, CToken)
genKeyword = liftM (\k -> (k, Keyword k)) keyword
  where keyword = elements allCKeywords

genIdent :: Gen (String , CToken)
genIdent = liftM (\i -> (i, Identifier i)) ident
  where ident    = liftM2 (:) nonDigit rest 
        nonDigit = elements cNonDigit 
        rest     = listOf $ elements (cNonDigit ++ cDigit)

genDecConst :: Gen (String, CToken)
genDecConst = liftM (\d -> (show d, DecConstant d)) decConst
  where decConst     = liftM (read :: String -> Integer) decConstStr 
        decConstStr  = liftM2 (:) nonZeroDigit digits
        nonZeroDigit = elements cNonZeroDigit
        digits       = listOf $ elements cDigit

-- | arbitary :: Gen CChar generates awful escaped characters.
-- | (so, by extension, genStringLit generates awful strings). Maybe
-- | it would be best to just limit the strings to mostly alphanumeric for readability.
genCharConstant :: Gen (String, CToken)
genCharConstant = liftM (\c -> (show c, CharConstant c)) char
  where char  = liftM castCCharToChar cChar 
        cChar = arbitrary :: Gen CChar

genStringLit :: Gen (String, CToken)
genStringLit = liftM (\s -> (s, StringLit s)) str 
  where str   = listOf char
        char  = liftM castCCharToChar cChar 
        cChar = arbitrary :: Gen CChar

genPunctuator :: Gen (String, CToken)
genPunctuator = liftM (\p -> (p, Punctuator p)) punctuator
  where punctuator = elements allCPunctuators 

genWhitespace :: Gen String
genWhitespace = resize 5 $ listOf1 
  (frequency[(10, return ' ')
            ,(1, elements cWhitespace)
            ]
  )

genCommentBlock :: Gen String
genCommentBlock = liftM2 (++) (liftM2 (++) start middle) end
  where end    = return "*/"
        middle = suchThat str $ notInfixOf "*/"
        start  = return "/*"
        str    = arbitrary :: Gen String
        notInfixOf =  \xs ys -> not $ isInfixOf xs ys

genCommentInline :: Gen String
genCommentInline = liftM2 (++) (liftM2 (++) start middle) end
  where end    = return "\n"
        middle = suchThat str $ notInfixOf "\n"
        start  = return "//"
        str    = arbitrary :: Gen String
        notInfixOf =  \xs ys -> not $ isInfixOf xs ys

genComment :: Gen String
genComment = oneof [genCommentBlock, genCommentInline]

-- | Tokens are generated a little rigidly (and not comprehensively).
-- | The structure is token - whitespace - maybe a comment - more whitespace. 
-- | Probably can be improved.
genToken :: Gen (String, CToken)
genToken = do
  (s, t) <-  oneof [ genKeyword, genIdent, genDecConst
                   , genCharConstant, genStringLit
                   , genPunctuator]
  ws1     <- genWhitespace
  comment <- frequency[(10, return ""), (1, genComment )]
  ws2     <- genWhitespace
  return (s ++ ws1 ++ comment ++ ws2, t)

genCFile :: Gen (String, [CToken])
genCFile = do
  stPairs <- resize 50 $ listOf1 genToken
  let f (accS, accT) (s, t) = (accS ++ s, accT ++ [t]) 
  let cFile = foldl f ("", []) stPairs
  return cFile 

-- | Testing
newtype KeywordG = KeywordG (String, CToken)
  deriving (Show, Eq)

newtype CFileG = CFileG (String, [CToken])
  deriving (Show, Eq)

instance Arbitrary KeywordG where
  arbitrary = liftM KeywordG genKeyword

instance Arbitrary CFileG where
  arbitrary = liftM CFileG genCFile
  
prop_genKeyword keywordPair = Right [t] == runLexer_ s
 where KeywordG (s, t) = keywordPair
       types = keywordPair :: KeywordG

prop_genCFile filePair = Right ts == runLexer_ s
 where CFileG (s, ts) = filePair
       types = filePair :: CFileG




