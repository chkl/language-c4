{-# LANGUAGE OverloadedStrings #-}
module CLangDef where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.List       (sortBy)
import           Data.Ord        (comparing)
import           Data.Word       (Word8)

-- don't use this on dynamic data, it's only a partial function
w :: Char -> Word8
w = fromIntegral.fromEnum


allCKeywords :: [ByteString]
allCKeywords = ["auto", "if", "unsigned", "break", "inline", "void", "case",
                "int", "volatile", "char", "long", "while", "const", "register", "_Alignas",
                "continue", "restrict", "_Alignof", "default", "return", "_Atomic", "do",
                "short", "_Bool", "double", "signed", "_Complex", "else", "sizeof", "_Generic",
                "enum", "static", "_Imaginary", "extern", "struct", "_Noreturn", "float",
                "switch", "_Static_assert", "for", "typedef", "_Thread_local", "goto", "union"]


allCPunctuators :: [ByteString]
allCPunctuators = sortBy (flip $ comparing BS.length) $
  map (BS.singleton . w) ".&*+-~!/%<>^|?:;=,#" ++
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


-- | a simple map that maps the second character of a simple escape sequence
-- | (e.g. the n in "\n") to the actual character that is represented by the
-- | escape sequence.
cSimpleEscapeSequences :: [(Word8, Word8)]
cSimpleEscapeSequences =  [ (w '\'',w  '\'')
                          , (w '"', w '"')
                          , (w '?', w '?' )
                          , (w '\\',w  '\\')
                          , (w 'a', w '\a')
                          , (w 'b', w '\b')
                          , (w 'f', w '\f')
                          , (w 'n', w '\n')
                          , (w 'r', w '\r')
                          , (w 't', w '\t')
                          , (w 'v', w '\v') ]
