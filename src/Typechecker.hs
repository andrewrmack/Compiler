{-# LANGUAGE BangPatterns, OverloadedStrings, TupleSections #-}
module Typechecker(typecheck, getType) where

import Control.Monad.State
import qualified Data.HashMap.Lazy as H
import Data.HashMap.Lazy (HashMap)

import Builtin
import Language.Expression
import Language.Type
import Utility.Basic
import Utility.Error
import Utility.PrettyPrint
import Utility.Location

getType :: Expr -> Type
getType = fst . runTypecheck

typecheck :: Expr -> Expr
typecheck = snd . runTypecheck
{-# INLINE typecheck #-}

runTypecheck :: Expr-> (Type, Expr)
runTypecheck e = case evalState (typecheck' H.empty e) 0 of
                     (!t, !e') -> (t, e') -- Force t and e to WHNF

type Gamma = HashMap Name Type

freshGenSym :: State Int Type
freshGenSym = do
  i <- get
  modify (+1)
  return $ TyGenSym i

typecheck' :: Gamma -> Expr -> State Int (Type, Expr)
typecheck' _ e@EEmpty{} = (,e) <$> freshGenSym
typecheck' _ e@EInt{} = return (TyLit "Int", e)
typecheck' _ e@EFloat{} = return (TyLit "Float", e)
typecheck' _ e@EBool{} = return (TyLit "Bool", e)
typecheck' g e@EVar{} = case H.lookup (evar e) g of
                          Just t -> return (t, e)
                          Nothing -> case lookup (evar e) builtins of
                                       Just t -> return (t, e)
                                       Nothing -> locatedError (locate e) $
                                          "Variable " <> evar e <> " not in scope"
typecheck' g e@ECons{} = do
  (t1,e1) <- typecheck' g (eelem e)
  (t2,e2) <- typecheck' g (elist e)
  case unify (TyList t1) t2 of
    Just t3 -> return (t3, e{eelem=e1,elist=e2})
    Nothing -> locatedError (locate e) "Can't cons invalid list"
typecheck' g e@ESig{} = do
  let t1 = etype e
  (t2,e') <- typecheck' g (eexp e)
  case unify t1 t2 of
    Just t -> return (t, e')
    Nothing -> locatedError (locate e) $
      "Failed to unify signature " <> ppr t1 <> " with actual type " <> ppr t2
typecheck' g e@ETuple{} = do
  (ts, es') <- unzip <$> mapM (typecheck' g) (eelems e)
  return (TyTuple ts, e{eelems=es'})
typecheck' g e@EList{} = do
  (ts, es') <- unzip <$> mapM (typecheck' g) (eelems e)
  tmp <- freshGenSym
  case foldM unify tmp ts of
    Just t' -> return (TyList t', e{eelems=es'})
    Nothing -> locatedError (locate e) "Failed to unify heterogenous list"
typecheck' g e@EApp{} = do
  (t1,e1) <- typecheck' g (eapp1 e)
  (t2,e2) <- typecheck' g (eapp2 e)
  tmp1 <- freshGenSym
  tmp2 <- freshGenSym
  case unify t1 (TyArr tmp1 tmp2) of
    Just (TyArr t3 t4) ->
      case unify t2 t3 of
        Just _ -> return (t4, EApp (eloc e) e1 e2)
        _ -> locatedError (locate e) $
               "Failed to unify input " <> ppr t2 <> " with expected " <> ppr t3
    _ -> locatedError (locate e) $
           "Attempted to apply non-function type " <> ppr t1
typecheck' g e@EOp{} = do
  (t1,e1) <- typecheck' g (eopp1 e)
  (t2,e2) <- typecheck' g (eopp2 e)
  let t = case (unify t1 t2, unify t1 (TyLit "Int"), unify t1 (TyLit "Float")) of
            (Just _, Just _, _) -> opType (TyLit "Int") (eop e)
            (_, Just _, _) -> locatedError (locate (eopp2 e)) $
              "Unexpected type " <> ppr t2 <> ". Expected Int"
            (Just _, _, Just _) -> opType (TyLit "Float") (eop e)
            (_, _, Just _) -> locatedError (locate (eopp2 e)) $
              "Unexpected type " <> ppr t2 <> ". Expected Float"
            _ -> locatedError (locate (eopp1 e)) $
              "Unexpected type " <> ppr t1 <> ". Expected Float"
  return (t, e{eopp1=e1,eopp2=e2})
typecheck' g e@EIf{} = do
  (t1,e1) <- typecheck' g (eif e)
  (t2,e2) <- typecheck' g (ethen e)
  (t3,e3) <- typecheck' g (eelse e)
  case (t1, unify t2 t3) of
    (TyLit "Bool", Just t) -> return (t, e{eif=e1,ethen=e2,eelse=e3})
    (TyLit "Bool", Nothing) -> locatedError (locate e) $
      "Failed to unify type " <> ppr t2 <> " with " <> ppr t3
    (t, _) -> locatedError (locate e) $
      "Expected Bool in guard of conditional. Given " <> ppr t
typecheck' g e@ELet{} = do
  (t1,e1) <- typecheck' g (ebind e)
  (t2,e2) <- typecheck' (H.insert (ename e) t1 g) (ein e)
  return (t2, e{ein=e2,ebind=e1})
typecheck' g e@EFix{} = do
  t1 <- freshGenSym
  t2 <- freshGenSym
  (t,e') <- typecheck' (H.insert (efun e) (TyArr t1 t2) (H.insert (evar e) t1 g)) (ebody e)
  return (TyArr t1 t, e{ebody=e'})
typecheck' g e@ELam{} = do
  t1 <- freshGenSym
  (t,e') <- typecheck' (H.insert (evar e) t1 g) (ebody e)
  return (TyArr t1 t, e{ebody=e'})

unify :: Type -> Type -> Maybe Type
unify t1 t2
  -- Equal types are already unified
  | t1 == t2     = Just t1
  -- A Type variable unifies with anything
  | TyVar _ <- t1 = Just t2
  | TyVar _ <- t2 = Just t1
  -- GenSyms are just type variables we made up
  | TyGenSym _ <- t1 = Just t2
  | TyGenSym _ <- t2 = Just t1
  -- Start pattern matching
  | otherwise =
      case (t1, t2) of
        -- Arrows unify if their parts unify
        (TyArr t11 t12, TyArr t21 t22) -> TyArr <$> unify t11 t21 <*> unify t12 t22
        (TyList t1', TyList t2') -> TyList <$> unify t1' t2'
        -- Tuples unify if their types unify
        -- zipWithM will let you get away with different lengths, though
        (TyTuple t1s, TyTuple t2s) -> if length t1s == length t2s
                                      then TyTuple <$> zipWithM unify t1s t2s
                                      else Nothing
        -- If we can't unify, fail
        _ -> Nothing
