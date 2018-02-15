module Interpreter (evaluate, interpret) where

import Data.ByteString.Lazy     (ByteString)
import Data.Text                (Text)
import Error
import Lexer
import Parser
import Lang

evaluate :: ByteString -> Text
evaluate = ppValue . interpret . parse . lexer
{-# INLINE evaluate #-}

interpret :: Expr Located -> Value
interpret EEmpty = VEmpty
interpret (EVar l _) = locatedError l "Cannot evaluate unbound variable"
interpret (EInt _ n) = VInt n
interpret (EBool _ b) = VBool b
interpret (EFloat _ f) = VFloat f
interpret (ELet _ n e1 e2) = interpret $ substitute n e1 e2
interpret (EIf l e1 e2 e3) = if b1 then interpret e2 else interpret e3
  where
    b1 = case interpret e1 of
           (VBool b) -> b
           _ -> locatedError l "Cannot evaluate 'if' with non-boolean condition"
interpret (EOp l op e1 e2) =
  case (interpret e1, interpret e2) of
    (VFloat f1, VFloat f2) -> floatOp op f1 f2
    (VFloat f1, VInt n2)   -> floatOp op f1 (fromIntegral n2)
    (VInt n1, VFloat f2)   -> floatOp op (fromIntegral n1) f2
    (VInt n1, VInt n2)     -> intOp l op n1 n2
    _ -> locatedError l "Cannot perform arithmetic operation on non number"

substitute :: Name -> Expr a -> Expr a -> Expr a
substitute _ _ EEmpty = EEmpty
substitute _ _ e@(EInt _ _) = e
substitute _ _ e@(EFloat _ _) = e
substitute _ _ e@(EBool _ _) = e
substitute n e (ELet l x e1 e2) = ELet l x (substitute n e e1) (substitute n e e2)
substitute n e (EOp l o e1 e2) = EOp l o (substitute n e e1) (substitute n e e2)
substitute n e (EIf l e1 e2 e3) = EIf l (substitute n e e1)
                                        (substitute n e e2)
                                        (substitute n e e3)
substitute n e e'@(EVar l x)
  | x == n = e
  | otherwise = e'

floatOp :: Op -> Double -> Double -> Value
floatOp Plus   f1 f2 = VFloat $ f1 +  f2
floatOp Minus  f1 f2 = VFloat $ f1 -  f2
floatOp Times  f1 f2 = VFloat $ f1 *  f2
floatOp Divide f1 f2 = VFloat $ f1 /  f2
floatOp Lte    f1 f2 = VBool  $ f1 <= f2
{-# INLINE floatOp #-}

intOp :: Located -> Op -> Int -> Int -> Value
intOp _ Plus  n1 n2  = VInt  $ n1 +  n2
intOp _ Minus n1 n2  = VInt  $ n1 -  n2
intOp _ Times n1 n2  = VInt  $ n1 *  n2
intOp _ Lte   n1 n2  = VBool $ n1 <= n2
intOp l Divide n1 n2
  | n2 == 0   = locatedError l "Error: Divide by zero"
  | otherwise = VInt $ n1 `quot` n2
{-# INLINE intOp #-}
