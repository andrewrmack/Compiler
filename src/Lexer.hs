{-# LANGUAGE DeriveGeneric #-}
module Lexer (lexer, Token(..)) where

import Control.DeepSeq (NFData)
import Data.Char       (isSpace, isDigit)
import qualified Data.Text as T
import Data.Text.Read
import GHC.Generics    (Generic)

data Token =
    TLParen
  | TRParen
  | TOp (Integer -> Integer -> Integer)
  | TInt Integer
  deriving (Generic)

instance NFData Token

lexer :: T.Text -> [Token]
lexer txt =
  case T.uncons txt of
    Nothing -> []
    Just ('(', txt') -> TLParen  : lexer txt'
    Just (')', txt') -> TRParen  : lexer txt'
    Just ('+', txt') -> TOp (+)  : lexer txt'
    Just ('-', txt') -> TOp (-)  : lexer txt'
    Just ('*', txt') -> TOp (*)  : lexer txt'
    Just ('/', txt') -> TOp div' : lexer txt'
    Just (t,   txt') -> let go | isSpace t = lexer (T.stripStart txt')
                               | isDigit t = lexInt txt
                               | otherwise = error $ "Can't lex character '" ++ t : "'"
                         in go

lexInt :: T.Text -> [Token]
lexInt txt = TInt num : lexer rest
  where
    (num, rest) = case decimal txt of
                    Right (n, r) -> (n, r)
                    Left _       -> error $ "Can't lex '" ++ T.unpack txt ++ "' (you should never see this error)"

div' :: (Integral a) => a -> a -> a
div' m n
  | n == 0    = error "Error: Divide by zero"
  | otherwise = div m n
