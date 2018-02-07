{-# LANGUAGE DeriveGeneric, OverloadedStrings, RankNTypes #-}
module Lexer
  ( lexer
  , Token(..)
  , Op(..)
  ) where

import Control.DeepSeq (NFData)
import Data.Char       (isSpace, isDigit, isAlpha)
import qualified Data.Text as T
import Data.Text.Read
import GHC.Generics    (Generic)

data Op = Plus | Minus | Times | Divide deriving (Generic)

data Token =
    TLParen
  | TRParen
  | TLte
  | TIf
  | TOp    !Op
  | TBool  !Bool
  | TInt   {-# UNPACK #-} !Int
  | TFloat {-# UNPACK #-} !Double
  deriving (Generic)

instance NFData Op
instance NFData Token

lexer :: T.Text -> [Token]
lexer txt =
  case T.uncons txt of
    Nothing -> []
    Just ('(', txt') -> TLParen    : lexer txt'
    Just (')', txt') -> TRParen    : lexer txt'
    Just ('+', txt') -> TOp Plus   : lexer txt'
    Just ('-', txt') -> TOp Minus  : lexer txt'
    Just ('*', txt') -> TOp Times  : lexer txt'
    Just ('/', txt') -> TOp Divide : lexer txt'
    Just ('<', txt') -> case T.uncons txt' of
                          Just ('=', txt'') -> TLte : lexer txt''
                          _ -> errorWithoutStackTrace "Can't lex '<'"
    Just (t,   txt') -> let go | isSpace t = lexer (T.stripStart txt')
                               | isDigit t = lexNum  txt
                               | isAlpha t = lexWord txt
                               | otherwise = errorWithoutStackTrace $ "Can't lex '" ++ t : "'"
                         in go

lexWord :: T.Text -> [Token]
lexWord txt
  | word == "if"    = TIf : lexer txt'
  | word == "true"  = TBool True  : lexer txt'
  | word == "false" = TBool False : lexer txt'
  | otherwise       = errorWithoutStackTrace $ "unrecognized text '" ++ T.unpack word ++ "'"
  where
    (word, txt') = T.span isAlpha txt
{-# INLINE lexWord #-}

lexNum :: T.Text -> [Token]
lexNum txt
  | dots == 1 = TFloat float : lexer rest
  | dots == 0 = TInt int : lexer rest
  | otherwise = lexNumError
  where
    (num, rest) = T.span (\c -> isDigit c || c == '.') txt
    lexNumError = errorWithoutStackTrace $ "couldn't lex number '" ++ T.unpack num ++ "'"
    dots = T.count "." num
    int = case decimal num of
            Right (i, "") -> i
            _ -> lexNumError
    float = case double num of
              Right (f, "") -> f
              _ -> lexNumError
