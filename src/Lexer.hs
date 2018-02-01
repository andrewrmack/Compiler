module Lexer (lexer, Token(..)) where

import Data.Char (isSpace, isDigit)

data Token =
    TLParen
  | TRParen
  | TPlus
  | TInt Integer
  deriving (Show, Eq)

lexer :: String -> [Token]
lexer ""       = []
lexer ('(':ss) = TLParen : lexer ss
lexer (')':ss) = TRParen : lexer ss
lexer ('+':ss) = TPlus   : lexer ss
lexer str@(s:ss)
  | isSpace s = lexer ss
  | isDigit s = lexInt str
  | otherwise = error $ "Unparseable character '" ++ [s] ++ "' encountered"

lexInt :: String -> [Token]
lexInt str = TInt (read num) : lexer rest
  where
    (num, rest) = span isDigit str
