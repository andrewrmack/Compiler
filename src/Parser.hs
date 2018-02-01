module Parser (parse, Expr(..)) where

import Lexer

data Expr =
    EInt Integer
  | EAdd Expr Expr
  deriving (Show, Eq)

parse :: [Token] -> Expr
parse ts =
  case parse' ts of
    (e, []) -> e
    (_, ts) -> error "Trailing tokens founds"

parse' :: [Token] -> (Expr, [Token])
parse' (TInt n : ts) = (EInt n, ts)
parse' (TLParen:TPlus:ts) =
  case ts'' of
    TRParen:ts''' -> (EAdd e1 e2, ts''')
    _             -> error "Missing closing )"
  where
    (e1, ts')  = parse' ts
    (e2, ts'') = parse' ts'
parse' _ = error "Cannot parse token stream"
