{-# LANGUAGE OverloadedStrings #-}
module Interpreter (evaluate, interpret) where

import Control.Monad            (when)
import Data.Maybe               (isJust)
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
  v <- simplify [] e
  if isFinal v
  then return v
  else locatedError (locate e) "Expression could not be reduced to a value"

warnNameShadow :: Location -> Name -> Compiler ()
warnNameShadow l n = logWarning l $ "Binding of " <> n <> " shadows existing binding"

addBinding :: Location -> Name -> Value -> Env -> Compiler Env
addBinding l n v e = do
  when (isJust (lookup n e)) (warnNameShadow l n)
  return $ (n,v):e

-- Until I implement full pattern matching, these have to be
-- special cases
interpretBuiltinApp :: Location -> Name -> Value -> Compiler Value
interpretBuiltinApp l "fst" v =
  case v of
    VTuple vs -> return $ head vs
    _ -> locatedError l "Cannot get first of non-tuple"
interpretBuiltinApp l "snd" v =
  case v of
    VTuple vs -> return $ head (tail vs)
    _ -> locatedError l "Cannot get second of non-tuple"
interpretBuiltinApp l "empty" v =
  case v of
    VList vs -> return $ VBool (null vs)
    _ -> locatedError l "Cannot check emptiness of non-list"
interpretBuiltinApp l "head" v =
  case v of
    VList vs -> return $ head vs
    _ -> locatedError l "Cannot get head of non-list"
interpretBuiltinApp l "tail" v =
  case v of
    VList vs -> return $ VList (tail vs)
    _ -> locatedError l "Cannot get tail of non-list"
interpretBuiltinApp l s _ = locatedError l $ s <> " is not a builtin function"

simplify :: Env -> Expr -> Compiler Value
simplify _ (EEmpty _)   = return VEmpty
simplify _ (EInt _ n)   = return $ VInt n
simplify _ (EBool _ b)  = return $ VBool b
simplify _ (EFloat _ f) = return $ VFloat f
simplify env (ELam _ x e) = return $ VLam x e env
simplify env (EFix _ f x e) = return $ VFix f x e env
simplify env (ELet l n e1 e2) = do
  v1 <- simplify env e1
  env' <- addBinding l n v1 env
  simplify env' e2
simplify env (ESig _ e _) = simplify env e
simplify env (EVar _ n)   =
  case lookup n env of
    Just v -> return v
    Nothing -> return $ VVar n
simplify env (ECons l e es) = do
  vs <- simplify env es
  v  <- simplify env e
  case vs of
    (VList vs') -> return $ VList (v:vs')
    _ -> locatedError l "Can't cons onto non list"
simplify env (ETuple _ es) = VTuple <$> mapM (simplify env) es
simplify env (EList _ es)  = VList  <$> mapM (simplify env) es
simplify env (EApp l e1 e2) = do
  v1 <- simplify env e1
  v2 <- simplify env e2
  case v1 of
    VLam n e env' -> do
      env'' <- addBinding l n v2 env'
      simplify env'' e
    VFix f x e env' -> do
      env'' <- addBinding l x v2 env'
      env''' <- addBinding l f (VFix f x e env') env''
      simplify env''' e
    VVar v -> interpretBuiltinApp l v v2
    _ -> locatedError l "Cannot apply non-lambda to expression"
simplify env (EIf l e1 e2 e3) = do
  v1 <- simplify env e1
  let b1 = case v1 of
             (VBool b) -> b
             _ -> locatedError l "Cannot evaluate 'if' with non-boolean condition"
  if b1 then simplify env e2 else simplify env e3
simplify env (EOp l op e1 e2) = do
  v1 <- simplify env e1
  v2 <- simplify env e2
  case (v1, v2) of
    (VFloat f1, VFloat f2) -> return $ floatOp l op f1 f2
    (VInt n1, VInt n2)     -> return $ intOp l op n1 n2
    _ -> locatedError l $
      "Cannot perform arithmetic operation on " <> ppr v1 <> " and " <> ppr v2


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
