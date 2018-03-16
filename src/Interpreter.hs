{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Interpreter (evaluate, interpret, initialEnv) where

import Control.Monad.State
import Data.ByteString.Lazy     (ByteString)
import Data.Maybe
import Data.Monoid              ((<>))
import Data.Text                (Text)
import qualified Data.IntMap as M
import qualified Data.Vector as V
import Error
import Lexer
import Parser
import Lang
import Location
import Typechecker

data Env = Env
  { _refs :: M.IntMap Expr
  , _arrs :: M.IntMap (V.Vector Expr)
  , _rint :: {-# UNPACK #-} !Int
  , _aint :: {-# UNPACK #-} !Int
  }

initialEnv :: Env
initialEnv = Env M.empty M.empty 0 0

lookupRef :: MonadState Env m => Int -> m (Maybe Expr)
lookupRef n = M.lookup n . _refs <$> get

updateRef :: MonadState Env m => Int -> Expr -> m ()
updateRef n e = modify $ \s -> s{_refs=M.insert n e (_refs s)}

lookupArrAtIndex :: MonadState Env m => Int -> Int -> m (Maybe Expr)
lookupArrAtIndex n i = join . fmap (V.!? i) . M.lookup n . _arrs <$> get

getNewRef :: MonadState Env m => m Int
getNewRef = do
  ref <- _rint <$> get
  modify $ \s -> s{_rint=ref+1}
  return ref

getNewArrRef :: MonadState Env m => m Int
getNewArrRef = do
  ref <- _aint <$> get
  modify $ \s -> s{_aint=ref+1}
  return ref

evaluate :: ByteString -> Compiler Text
evaluate = fmap ppValue . flip evalStateT initialEnv . interpret . typecheck . parse . lexer
{-# INLINE evaluate #-}

interpret :: Expr -> StateT Env (State CompilerState) Value
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
interpretBuiltinApp :: Name -> Expr -> StateT Env (State CompilerState) Expr
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

-- The type synonyms can't be partially applied, unfortunately
simplify :: Expr -> StateT Env (State CompilerState) Expr
simplify e@(EEmpty _)   = return e
simplify v@(EVar _ _)   = return v
simplify n@(EInt _ _)   = return n
simplify b@(EBool _ _)  = return b
simplify f@(EFloat _ _) = return f
simplify e@(EPoint _ _) = return e
simplify (ESig _ e _)   = simplify e
simplify (ESeq _ e1 e2) = do
  _ <- simplify e1
  simplify e2
simplify (EAssign l e1 e2) = do
  e1' <- simplify e1
  case e1' of
    (EPoint l' n) -> do
      updateRef n e2
      return $ EPoint l' n
    _ -> locatedError l "Cannot assign to non pointer"
simplify (EDeref l e) = do
  e' <- simplify e
  case e' of
    (EPoint _ n) -> fromMaybe (locatedError l ("Could not dereference " <> ppExpr e'))
                      <$> lookupRef n
    _ -> locatedError l "Could not dereference non-pointer"
simplify (ERef l e) = do
  ref <- getNewRef
  updateRef ref e
  return $ EPoint l ref
simplify (ECons l e es) = do
  es' <- simplify es
  case es' of
    (EList _ es'') -> simplify (EList l (e:es''))
    _ -> locatedError l "Can't cons onto non list"
simplify (ETuple l es)  = ETuple l <$> mapM simplify es
simplify (EList l es)   = EList  l <$> mapM simplify es
simplify (ELet _ n e1 e2) = do
  e1' <- simplify e1 -- It is necessary to force e1 to introduce refs now
  e2' <- substitute n e1' e2
  simplify e2'
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
      "Cannot perform arithmetic operation on " <> ppExpr e1' <> " and " <> ppExpr e2'

substitute :: Name -> Expr -> Expr -> StateT Env (State CompilerState) Expr
substitute _ _ e@(EEmpty _)    = return e
substitute _ _ e@(EInt _ _)    = return e
substitute _ _ e@(EFloat _ _)  = return e
substitute _ _ e@(EBool _ _)   = return e
substitute _ _ e@ENewArr{}     = return e
substitute n e (ESig _ e' _)   = substitute n e e'
substitute n e (EDeref l e')   = EDeref l <$> substitute n e e'
substitute n e (ERef l e')     = ERef l <$> substitute n e e'
substitute n e (EAssign l e1 e2) = EAssign l <$> substitute n e e1 <*> substitute n e e2
substitute n e (ESeq l e1 e2)  = ESeq l <$> substitute n e e1 <*> substitute n e e2
substitute n e (ECons l e' es) = ECons l <$> substitute n e e' <*> substitute n e es
substitute n e (ETuple l es) = ETuple l <$> mapM (substitute n e) es
substitute n e (EList  l es) = EList  l <$> mapM (substitute n e) es
substitute n e (EWhile l e1 e2) = EWhile l <$> substitute n e e1 <*> substitute n e e2
substitute n e (EArrAcc l e1 e2) = EArrAcc l <$> substitute n e e1 <*> substitute n e e2
substitute n e (ELam l x e1)
  | x == n    = pure (warnNameShadow l x) >> return (ELam l x e1)
  | otherwise = do
    e1' <- substitute n e e1
    return $ ELam l x e1'
substitute n e (EFix l f x e1)
  | x == n || x == f = pure (warnNameShadow l x) >> return (EFix l f x e1)
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
  | x == n = pure (warnNameShadow l x) >> return (ELet l x e1 e2)
  | otherwise = do
    e1' <- substitute n e e1
    e2' <- substitute n e e2
    return $ ELet l x e1' e2'
substitute n e e'@(EVar _ x)
  | x == n    = return e
  | otherwise = return e'

warnNameShadow :: Location -> Name -> Compiler ()
warnNameShadow l n = logWarning l $ "Binding of " <> n <> " shadows existing binding"

floatOp :: Location -> Op -> Double -> Double -> Expr
floatOp l Plus   f1 f2 = EFloat l $ f1 +  f2
floatOp l Minus  f1 f2 = EFloat l $ f1 -  f2
floatOp l Times  f1 f2 = EFloat l $ f1 *  f2
floatOp l Divide f1 f2 = EFloat l $ f1 /  f2
floatOp l Lte    f1 f2 = EBool  l $ f1 <= f2
{-# INLINE floatOp #-}

intOp :: Location -> Op -> Int -> Int -> Expr
intOp l Plus  n1 n2  = EInt  l $ n1 +  n2
intOp l Minus n1 n2  = EInt  l $ n1 -  n2
intOp l Times n1 n2  = EInt  l $ n1 *  n2
intOp l Lte   n1 n2  = EBool l $ n1 <= n2
intOp l Divide n1 n2
  | n2 == 0   = locatedError l "Error: Divide by zero"
  | otherwise = EInt l $ n1 `quot` n2
{-# INLINE intOp #-}
