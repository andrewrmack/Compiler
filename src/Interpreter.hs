module Interpreter (evaluate, interpret) where

import Data.ByteString.Lazy     (ByteString)
import Data.Text                (Text, unpack)
import Error
import Lexer
import Parser
import Lang
import Location

evaluate :: ByteString -> Compiler Text
evaluate = fmap ppValue . interpret . parse . lexer
{-# INLINE evaluate #-}

interpret :: Expr Location -> Compiler Value
interpret e = do
  e' <- simplify e
  case e' of
    EEmpty _   -> return VEmpty
    EInt _ n   -> return $ VInt n
    EBool _ b  -> return $ VBool b
    EFloat _ f -> return $ VFloat f
    ETuple _ es -> VTuple <$> mapM interpret es
    EList  _ es -> VList  <$> mapM interpret es
    e'' -> locatedError (locate e'') "Expression could not be reduced to a value"

simplify :: Expr Location -> Compiler (Expr Location)
simplify e@(EEmpty _)   = return e
simplify (ESig _ e _)   = simplify e
simplify v@(EVar _ _)   = return v
simplify n@(EInt _ _)   = return n
simplify b@(EBool _ _)  = return b
simplify f@(EFloat _ _) = return f
simplify (ETuple l es)  = ETuple l <$> mapM simplify es
simplify (EList l es)   = EList  l <$> mapM simplify es
simplify (ELet _ n e1 e2) = substitute n e1 e2 >>= simplify
simplify l@ELam{} = return l
simplify l@EFix{} = return l
simplify (EApp l e1 e2) = do
  e1' <- simplify e1
  case e1' of
    ELam _ n e -> substitute n e2 e >>= simplify
    EFix l' f x e -> substitute x e2 e >>= substitute f (EFix l' f x e) >>= simplify
    _ -> locatedError l "Cannot apply non-lambda to expression"
simplify (EIf l e1 e2 e3) = do
  e1' <- simplify e1
  let b1 = case e1' of { (EBool _ b) -> b; _ -> locatedError l "Cannot evaluate 'if' with non-boolean condition" }
  if b1 then simplify e2 else simplify e3
simplify (EOp l op e1 e2) = do
  e1' <- simplify e1
  e2' <- simplify e2
  case (e1', e2') of
    (EFloat _ f1, EFloat _ f2) -> return $ floatOp l op f1 f2
    (EFloat _ f1, EInt _ n2)   -> return $ floatOp l op f1 (fromIntegral n2)
    (EInt _ n1, EFloat _ f2)   -> return $ floatOp l op (fromIntegral n1) f2
    (EInt _ n1, EInt _ n2)     -> return $ intOp l op n1 n2
    _ -> locatedError l "Cannot perform arithmetic operation on non number"

substitute :: Name -> Expr Location -> Expr Location -> Compiler (Expr Location)
substitute _ _ e@(EEmpty _)     = return e
substitute _ _ e@(EInt _ _)     = return e
substitute _ _ e@(EFloat _ _)   = return e
substitute _ _ e@(EBool _ _)    = return e
substitute n e (ESig _ e' _)    = substitute n e e'
substitute n e (ETuple l es) = ETuple l <$> mapM (substitute n e) es
substitute n e (EList  l es) = EList  l <$> mapM (substitute n e) es
substitute n e (ELam l x e1)
  | x == n    = warnNameShadow l x >> return (ELam l x e1)
  | otherwise = do
    e1' <- substitute n e e1
    return $ ELam l x e1'
substitute n e (EFix l f x e1)
  | x == n || x == f = warnNameShadow l x >> return (EFix l f x e1)
  | otherwise = substitute n e e1
substitute n e (EApp l e1 e2)   = do
  e1' <- substitute n e e1
  e2' <- substitute n e e2
  return $ EApp l e1' e2'
substitute n e (EOp l o e1 e2)  = do
  e1' <- substitute n e e1
  e2' <- substitute n e e2
  return $ EOp l o e1' e2'
substitute n e (EIf l e1 e2 e3) = do
  e1' <- substitute n e e1
  e2' <- substitute n e e2
  e3' <- substitute n e e3
  return $ EIf l e1' e2' e3'
substitute n e (ELet l x e1 e2)
  | x == n = warnNameShadow l x >> return (ELet l x e1 e2)
  | otherwise = do
    e1' <- substitute n e e1
    e2' <- substitute n e e2
    return $ ELet l x e1' e2'
substitute n e e'@(EVar _ x)
  | x == n    = return e
  | otherwise = return e'

warnNameShadow :: Location -> Name -> Compiler ()
warnNameShadow l n = logWarning l $ "Binding of " ++ unpack n ++ " shadows existing binding"

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
