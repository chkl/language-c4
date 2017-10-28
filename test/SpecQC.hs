import           Lexer
import           CLangDef
import           Control.Monad                    (liftM, liftM2)
import           Data.Char                        (digitToInt)
import           Foreign.C.String                 (castCCharToChar)
import           Foreign.C.Types                  (CChar)
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

genDecConst :: Gen (Integer, Token)
genDecConst = liftM (\d -> (d, DecConstant d)) decConst
  where decConst     = liftM (read :: String -> Integer) decConstStr 
        decConstStr  = liftM2 (:) nonZeroDigit digits
        nonZeroDigit = elements cNonZeroDigit
        digits       = listOf $ elements cDigit

-- | arbitary :: Gen CChar generates awful escaped characters.
-- | (so, by extension, genStringLit generates awful strings). Maybe
-- | it would be best to just limit the strings to mostly alphanumeric for readability.
genCharConstant :: Gen (Char, Token)
genCharConstant = liftM (\c -> (c, CharConstant c)) char
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

-- | TODO: Combine generators to produce a source file to parse. 





