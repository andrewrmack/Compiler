{-# LANGUAGE OverloadedStrings #-}
module Interpreter (evaluate, interpret) where

import Data.Text                (Text)
import Language.Expression
import Language.Value
import Utility.Basic
import Utility.Error
import Utility.PrettyPrint
import Utility.Location

evaluate :: Expr -> Compiler Text
evaluate = fmap ppr . interpret
{-# INLINE evaluate #-}

interpret :: Expr -> Compiler Value
interpret e = do
  v <- simplify e
  if isFinal v
  then return v
  else locatedError (locate e) "Expression could not be reduced to a value"

-- Until I implement full pattern matching, these have to be
-- special cases
interpretBuiltinApp :: Name -> Expr -> Compiler Value
interpretBuiltinApp "fst" e = do
  e2 <- simplify e
  case e2 of
    VTuple es -> return $ head es
    _ -> locatedError (locate e) "Cannot get first of non-tuple"
interpretBuiltinApp "snd" e = do
  e2 <- simplify e
  case e2 of
    VTuple es -> return $ head (tail es)
    _ -> locatedError (locate e) "Cannot get second of non-tuple"
interpretBuiltinApp "empty" e = do
  e2 <- simplify e
  case e2 of
    VList es -> return $ VBool (null es)
    _ -> locatedError (locate e) "Cannot check emptiness of non-list"
interpretBuiltinApp "head" e = do
  e2 <- simplify e
  case e2 of
    VList es -> return $ head es
    _ -> locatedError (locate e) "Cannot get head of non-list"
interpretBuiltinApp "tail" e = do
  e2 <- simplify e
  case e2 of
    VList es -> return $ VList (tail es)
    _ -> locatedError (locate e) "Cannot get tail of non-list"
interpretBuiltinApp s e = locatedError (locate e) $
  s <> " is not a builtin function"

simplify :: Expr -> Compiler Value
simplify (ESig _ e _) = simplify e
simplify (EEmpty _)   = return VEmpty
simplify (EVar _ n)   = return $ VVar n
simplify (EInt _ n)   = return $ VInt n
simplify (EBool _ b)  = return $ VBool b
simplify (EFloat _ f) = return $ VFloat f
simplify (ECons l e es) = do
  vs <- simplify es
  v  <- simplify e
  case vs of
    (VList vs') -> return $ VList (v:vs')
    _ -> locatedError l "Can't cons onto non list"
simplify (ETuple _ es)  = VTuple <$> mapM simplify es
simplify (EList _ es)   = VList  <$> mapM simplify es
simplify (ELet _ n e1 e2) = substitute n e1 e2 >>= simplify
simplify (ELam _ x e) = return $ VLam x e
simplify (EFix _ f x e) = return $ VFix f x e
simplify (EApp l e1 e2) = do
  v1 <- simplify e1
  case v1 of
    VLam n e -> substitute n e2 e >>= simplify
    VFix f x e -> substitute x e2 e >>= substitute f (EFix l f x e) >>= simplify
    VVar v -> interpretBuiltinApp v e2
    _ -> locatedError l "Cannot apply non-lambda to expression"
simplify (EIf l e1 e2 e3) = do
  v1 <- simplify e1
  let b1 = case v1 of
             (VBool b) -> b
             _ -> locatedError l "Cannot evaluate 'if' with non-boolean condition"
  if b1 then simplify e2 else simplify e3
simplify (EOp l op e1 e2) = do
  v1 <- simplify e1
  v2 <- simplify e2
  case (v1, v2) of
    (VFloat f1, VFloat f2) -> return $ floatOp l op f1 f2
    (VInt n1, VInt n2)     -> return $ intOp l op n1 n2
    _ -> locatedError l $
      "Cannot perform arithmetic operation on " <> ppr v1 <> " and " <> ppr v2

substitute :: Name -> Expr -> Expr -> Compiler Expr
substitute _ _ e@(EEmpty _)    = return e
substitute _ _ e@(EInt _ _)    = return e
substitute _ _ e@(EFloat _ _)  = return e
substitute _ _ e@(EBool _ _)   = return e
substitute n e (ESig _ e' _)   = substitute n e e'
substitute n e (ECons l e' es) = ECons l <$> substitute n e e' <*> substitute n e es
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
warnNameShadow l n = logWarning l $ "Binding of " <> n <> " shadows existing binding"

floatOp :: Location -> Name -> Double -> Double -> Value
floatOp _ "+"  f1 f2 = VFloat $ f1 +  f2
floatOp _ "-"  f1 f2 = VFloat $ f1 -  f2
floatOp _ "*"  f1 f2 = VFloat $ f1 *  f2
floatOp _ "/"  f1 f2 = VFloat $ f1 /  f2
floatOp _ "<=" f1 f2 = VBool  $ f1 <= f2
floatOp l s _ _ = locatedError l $ "Unknown binary operator " <> s
{-# INLINE floatOp #-}

intOp :: Location -> Name -> Int -> Int -> Value
intOp _ "+"  n1 n2  = VInt  $ n1 +  n2
intOp _ "-"  n1 n2  = VInt  $ n1 -  n2
intOp _ "*"  n1 n2  = VInt  $ n1 *  n2
intOp _ "<=" n1 n2  = VBool $ n1 <= n2
intOp l "/"  n1 n2
  | n2 == 0   = locatedError l "Error: Divide by zero"
  | otherwise = VInt $ n1 `quot` n2
intOp l s _ _ = locatedError l $ "Unknown binary operator " <> s
{-# INLINE intOp #-}
