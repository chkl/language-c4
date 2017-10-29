module CLangDef where

import           Text.Parsec.Char
import           Text.Parsec.Language
import           Text.Parsec.Pos                          (SourcePos)
import           Text.Parsec.Prim                         (Parsec, getPosition,
                                                           (<|>))
import           Text.Parsec.String
import qualified Text.Parsec.Token                        as T
import           Text.ParserCombinators.Parsec.Combinator

import           Data.List                                (sortBy)
import           Data.Ord                                 (comparing)


allCKeywords :: [String]
allCKeywords = ["auto", "if", "unsigned", "break", "inline", "void", "case",
                "int", "volatile", "char", "long", "while", "const", "register", "_Alignas",
                "continue", "restrict", "_Alignof", "default", "return", "_Atomic", "do",
                "short", "_Bool", "double", "signed", "_Complex", "else", "sizeof", "_Generic",
                "enum", "static", "_Imaginary", "extern", "struct", "_Noreturn", "float",
                "switch", "_Static_assert", "for", "typedef", "_Thread_local", "goto", "union"]


allCPunctuators :: [String]
allCPunctuators = sortBy (flip $ comparing length) $
  map (\s -> [s]) ".&*+-~!/%<>^|?:;=,#" ++
  ["->", "++", "--", "<<", ">>", "<=", ">=", "==", "!=", "&&", "||", "...", "*=",
  "/=", "%=", "+=", "-=", "<<=", ">>=", "&=", "^=", "|=", "##", "<:", ":>", "<%",
  "%>", "%:", "%:%:"]

cNonDigit :: [Char]
cNonDigit =  ['_'] ++ ['A'..'Z'] ++ ['a'..'z']

cDigit :: [Char]
cDigit = '0':cNonZeroDigit

cNonZeroDigit :: [Char]
cNonZeroDigit = ['1'..'9']

cWhitespace :: [Char]
cWhitespace = [' ',  '\t', '\n', '\r', '\f', '\v']

cLangDef :: LanguageDef s
cLangDef = emptyDef { T.commentStart = "/*"
                       , T.commentEnd = "*/"
                       , T.commentLine = "//"
                       , T.nestedComments = False
                       , T.identStart = letter <|> char '_'
                       , T.identLetter = alphaNum <|> char '_'
                       , T.reservedNames = allCKeywords
                       , T.opStart = anyChar
                       , T.opLetter = anyChar
                       , T.reservedOpNames = allCPunctuators
                       , T.caseSensitive = True
                       }

-- | a simple map that maps the second character of a simple escape sequence
-- | (e.g. the n in "\n") to the actual character that is represented by the
-- | escape sequence.
cSimpleEscapeSequences :: [(Char, Char)]
cSimpleEscapeSequences =  [ ('\'', '\'')
                          , ('"', '"')
                          , ('?', '?' )
                          , ('\\', '\\')
                          , ('a', '\a')
                          , ('b', '\b')
                          , ('f', '\f')
                          , ('n', '\n')
                          , ('r', '\r')
                          , ('t', '\t')
                          , ('v', '\v') ]
