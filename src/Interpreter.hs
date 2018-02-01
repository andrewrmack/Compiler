module Interpreter (evaluate, interpret) where

import Lexer
import Parser

evaluate :: String -> Integer
evaluate = interpret . parse . lexer

interpret :: Expr -> Integer
interpret (EAdd e1 e2) = interpret e1 + interpret e2
interpret (EInt n) = n
