{-# LANGUAGE DeriveGeneric #-}
module Parser (parse, Expr(..)) where

import Control.DeepSeq (NFData)
import GHC.Generics    (Generic)

import Lexer

data Expr =
    EInt Integer
  | EOp (Integer -> Integer -> Integer) Expr Expr
  deriving (Generic)

instance NFData Expr

parse :: [Token] -> Expr
parse ts =
  case parse' ts of
    (e, []) -> e
    (_, ts) -> error "Trailing tokens founds"

parse' :: [Token] -> (Expr, [Token])
parse' (TInt n : ts) = (EInt n, ts)
parse' (TLParen:TOp op:ts) =
  case ts'' of
    TRParen:ts''' -> (EOp op e1 e2, ts''')
    _             -> error "Missing closing )"
  where
    (e1, ts')  = parse' ts
    (e2, ts'') = parse' ts'
parse' _ = error "Cannot parse token stream"
