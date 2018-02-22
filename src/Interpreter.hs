module Interpreter (evaluate, interpret) where

import Data.ByteString.Lazy     (ByteString)
import Data.Text                (Text)
import Error
import Lexer
import Parser
import Lang
import Location

evaluate :: ByteString -> Text
evaluate = ppValue . interpret . parse . lexer
{-# INLINE evaluate #-}

interpret :: Expr Location -> Value
interpret e = case simplify e of
                EEmpty _   -> VEmpty
                EInt _ n   -> VInt n
                EBool _ b  -> VBool b
                EFloat _ f -> VFloat f
                e' -> locatedError (locate e') "Expression could not be reduced to a value"

simplify :: Expr Location -> Expr Location
simplify e@(EEmpty _) = e
simplify v@(EVar _ _) = v
simplify n@(EInt _ _) = n
simplify b@(EBool _ _) = b
simplify f@(EFloat _ _) = f
simplify (ELet _ n e1 e2) = simplify $ substitute n e1 e2
simplify l@ELam{} = l
simplify l@EFix{} = l
simplify (EApp l e1 e2) =
  case simplify e1 of
    ELam _ n e -> simplify $ substitute n e2 e
    EFix l' f x e -> simplify $ substitute f (EFix l' f x e) (substitute x e2 e)
    _ -> locatedError l "Cannot apply non-lambda to expression"
simplify (EIf l e1 e2 e3) = if b1 then simplify e2 else simplify e3
  where
    b1 = case simplify e1 of
           (EBool _ b) -> b
           _ -> locatedError l "Cannot evaluate 'if' with non-boolean condition"
simplify (EOp l op e1 e2) =
  case (simplify e1, simplify e2) of
    (EFloat _ f1, EFloat _ f2) -> floatOp l op f1 f2
    (EFloat _ f1, EInt _ n2)   -> floatOp l op f1 (fromIntegral n2)
    (EInt _ n1, EFloat _ f2)   -> floatOp l op (fromIntegral n1) f2
    (EInt _ n1, EInt _ n2)     -> intOp l op n1 n2
    _ -> locatedError l "Cannot perform arithmetic operation on non number"

substitute :: Name -> Expr a -> Expr a -> Expr a
substitute _ _ e@(EEmpty _)     = e
substitute _ _ e@(EInt _ _)     = e
substitute _ _ e@(EFloat _ _)   = e
substitute _ _ e@(EBool _ _)    = e
substitute n e (ELam l x e1)    = ELam l x $ if x == n then e1 else substitute n e e1
substitute n e (EFix l f x e1)  = EFix l f x $ if x == n || x == f then e1 else substitute n e e1
substitute n e (EApp l e1 e2)   = EApp l (substitute n e e1) (substitute n e e2)
substitute n e (EOp l o e1 e2)  = EOp l o (substitute n e e1) (substitute n e e2)
substitute n e (EIf l e1 e2 e3) = EIf l (substitute n e e1) (substitute n e e2) (substitute n e e3)
substitute n e (ELet l x e1 e2) = if x == n
                                  then ELet l x e1 e2
                                  else ELet l x (substitute n e e1) (substitute n e e2)
substitute n e e'@(EVar _ x)
  | x == n    = e
  | otherwise = e'

floatOp :: Location -> Op -> Double -> Double -> Expr Location
floatOp l Plus   f1 f2 = EFloat l $ f1 +  f2
floatOp l Minus  f1 f2 = EFloat l $ f1 -  f2
floatOp l Times  f1 f2 = EFloat l $ f1 *  f2
floatOp l Divide f1 f2 = EFloat l $ f1 /  f2
floatOp l Lte    f1 f2 = EBool  l $ f1 <= f2
{-# INLINE floatOp #-}

intOp :: Location -> Op -> Int -> Int -> Expr Location
intOp l Plus  n1 n2  = EInt  l $ n1 +  n2
intOp l Minus n1 n2  = EInt  l $ n1 -  n2
intOp l Times n1 n2  = EInt  l $ n1 *  n2
intOp l Lte   n1 n2  = EBool l $ n1 <= n2
intOp l Divide n1 n2
  | n2 == 0   = locatedError l "Error: Divide by zero"
  | otherwise = EInt l $ n1 `quot` n2
{-# INLINE intOp #-}
