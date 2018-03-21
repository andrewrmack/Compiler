{-# LANGUAGE OverloadedStrings #-}
module Interpreter (evaluate, interpret) where

import Data.ByteString.Lazy     (ByteString)
import Data.Text                (Text)
import Parser
import Language.Expression
import Language.Value
import Typechecker
import Utility.Basic
import Utility.Error
import Utility.PrettyPrint
import Utility.Location

evaluate :: ByteString -> Compiler Text
evaluate = fmap ppr . interpret . typecheck . parse
{-# INLINE evaluate #-}

interpret :: Expr -> Compiler Value
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

-- Until I implement full pattern matching, these have to be
-- special cases
interpretBuiltinApp :: Name -> Expr -> Compiler Expr
interpretBuiltinApp "fst" e = do
  e2 <- simplify e
  case e2 of
    (ETuple _ es) -> return $ head es
    _ -> locatedError (locate e) "Cannot get first of non-tuple"
interpretBuiltinApp "snd" e = do
  e2 <- simplify e
  case e2 of
    (ETuple _ es) -> return $ head (tail es)
    _ -> locatedError (locate e) "Cannot get second of non-tuple"
interpretBuiltinApp "empty" e = do
  e2 <- simplify e
  case e2 of
    (EList l es) -> return $ EBool l (null es)
    _ -> locatedError (locate e) "Cannot check emptiness of non-list"
interpretBuiltinApp "head" e = do
  e2 <- simplify e
  case e2 of
    (EList _ es) -> return $ head es
    _ -> locatedError (locate e) "Cannot get head of non-list"
interpretBuiltinApp "tail" e = do
  e2 <- simplify e
  case e2 of
    (EList l es) -> return $ EList l (tail es)
    _ -> locatedError (locate e) "Cannot get tail of non-list"
interpretBuiltinApp s e = locatedError (locate e) $
  s <> " is not a builtin function"

simplify :: Expr -> Compiler Expr
simplify e@(EEmpty _)   = return e
simplify (ESig _ e _)   = simplify e
simplify v@(EVar _ _)   = return v
simplify n@(EInt _ _)   = return n
simplify b@(EBool _ _)  = return b
simplify f@(EFloat _ _) = return f
simplify (ECons l e es) = do
  es' <- simplify es
  case es' of
    (EList _ es'') -> simplify (EList l (e:es''))
    _ -> locatedError l "Can't cons onto non list"
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
    EVar _ v -> interpretBuiltinApp v e2 >>= simplify
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
    (EInt _ n1, EInt _ n2)     -> return $ intOp l op n1 n2
    _ -> locatedError l $
      "Cannot perform arithmetic operation on " <> ppr e1' <> " and " <> ppr e2'

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

floatOp :: Location -> Name -> Double -> Double -> Expr
floatOp l "+"  f1 f2 = EFloat l $ f1 +  f2
floatOp l "-"  f1 f2 = EFloat l $ f1 -  f2
floatOp l "*"  f1 f2 = EFloat l $ f1 *  f2
floatOp l "/"  f1 f2 = EFloat l $ f1 /  f2
floatOp l "<=" f1 f2 = EBool  l $ f1 <= f2
floatOp l s _ _ = locatedError l $ "Unknown binary operator " <> s
{-# INLINE floatOp #-}

intOp :: Location -> Name -> Int -> Int -> Expr
intOp l "+"  n1 n2  = EInt  l $ n1 +  n2
intOp l "-"  n1 n2  = EInt  l $ n1 -  n2
intOp l "*"  n1 n2  = EInt  l $ n1 *  n2
intOp l "<=" n1 n2  = EBool l $ n1 <= n2
intOp l "/"  n1 n2
  | n2 == 0   = locatedError l "Error: Divide by zero"
  | otherwise = EInt l $ n1 `quot` n2
intOp l s _ _ = locatedError l $ "Unknown binary operator " <> s
{-# INLINE intOp #-}
