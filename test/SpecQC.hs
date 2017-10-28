import           Lexer
import           CLangDef
import           Control.Monad                    (liftM, liftM2, mapM, ap)
import           Data.Char                        (digitToInt)
import           Foreign.C.String                 (castCCharToChar)
import           Foreign.C.Types                  (CChar)
import           Data.List                        (isInfixOf)
import           Test.QuickCheck
import           Text.Parsec.Pos
import           Text.Parsec.Error
import           Text.Parsec.Prim

runLexer :: String -> Either ParseError [(Token, SourcePos)]
runLexer = runParser lexer () "test.c"

runLexer_ :: String -> Either ParseError [Token]
runLexer_ = fmap (map fst) . runLexer

-- | Each generator produces a pair consisting of a string (to parse) as well
-- | as the token that would result if that string were correctly parsed.
genKeyword :: Gen (String, Token)
genKeyword = liftM (\k -> (k, Keyword k)) keyword
  where keyword = elements allCKeywords

genIdent :: Gen (String , Token)
genIdent = liftM (\i -> (i, Identifier i)) ident
  where ident    = liftM2 (:) nonDigit rest 
        nonDigit = elements cNonDigit 
        rest     = listOf $ elements (cNonDigit ++ cDigit)

genDecConst :: Gen (String, Token)
genDecConst = liftM (\d -> (show d, DecConstant d)) decConst
  where decConst     = liftM (read :: String -> Integer) decConstStr 
        decConstStr  = liftM2 (:) nonZeroDigit digits
        nonZeroDigit = elements cNonZeroDigit
        digits       = listOf $ elements cDigit

-- | arbitary :: Gen CChar generates awful escaped characters.
-- | (so, by extension, genStringLit generates awful strings). Maybe
-- | it would be best to just limit the strings to mostly alphanumeric for readability.
genCharConstant :: Gen (String, Token)
genCharConstant = liftM (\c -> (show c, CharConstant c)) char
  where char  = liftM castCCharToChar cChar 
        cChar = arbitrary :: Gen CChar

genStringLit :: Gen (String, Token)
genStringLit = liftM (\s -> (s, StringLit s)) str 
  where str   = listOf char
        char  = liftM castCCharToChar cChar 
        cChar = arbitrary :: Gen CChar

genPunctuator :: Gen (String, Token)
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
genToken :: Gen (String, Token)
genToken = do
  (s, t) <-  oneof [ genKeyword, genIdent, genDecConst
                   , genCharConstant, genStringLit
                   , genPunctuator]
  ws1     <- genWhitespace
  comment <- frequency[(10, return ""), (1, genComment )]
  ws2     <- genWhitespace
  return (s ++ ws1 ++ comment ++ ws2, t)

genCFile :: Gen (String, [Token])
genCFile = do
  stPairs <- resize 40 $ listOf1 genToken
  let f (accS, accT) (s, t) = (accS ++ s, accT ++ [t]) 
  let cFile = foldl f ("", []) stPairs
  return cFile 
  
  



