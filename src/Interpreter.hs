module Interpreter (evaluate, interpret) where

import qualified Data.Text as T
import Lexer
import Parser

evaluate :: T.Text -> Integer
evaluate = interpret . parse . lexer

interpret :: Expr -> Integer
interpret (EOp op e1 e2) = interpret e1 `op` interpret e2
interpret (EInt n) = n
