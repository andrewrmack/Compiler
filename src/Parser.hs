{-# LANGUAGE Safe #-}
module Parser (parse, Expr(..), ppExpr) where

import Lang

parse :: [Token] -> Expr
parse ts =
  case parse' ts of
    (e, []) -> e
    _ -> errorWithoutStackTrace "Trailing tokens founds"
{-# INLINE parse #-}

parse' :: [Token] -> (Expr, [Token])
parse' (TInt n : ts)  = (EInt n, ts)
parse' (TFloat n : ts)  = (EFloat n, ts)
parse' (TBool b : ts) = (EBool b, ts)
parse' (TLParen:TOp op:ts) =
  case ts'' of
    TRParen:ts''' -> (EOp op e1 e2, ts''')
    _             -> errorWithoutStackTrace "Missing closing )"
  where
    (e1, ts')  = parse' ts
    (e2, ts'') = parse' ts'
parse' (TLParen:TLte:ts) =
  case ts'' of
    TRParen:ts''' -> (ELte e1 e2, ts''')
    _             -> errorWithoutStackTrace "Missing closing )"
  where
    (e1, ts')  = parse' ts
    (e2, ts'') = parse' ts'
parse' (TLParen:TIf:ts) =
  case ts''' of
    TRParen:ts'''' -> (EIf e1 e2 e3, ts'''')
    _             -> errorWithoutStackTrace "Missing closing )"
  where
    (e1, ts')    = parse' ts
    (e2, ts'')   = parse' ts'
    (e3, ts''')  = parse' ts''

parse' _ = errorWithoutStackTrace "Cannot parse token stream"
