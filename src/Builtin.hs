{-# LANGUAGE OverloadedStrings #-}
module Builtin where

import Data.Text (Text)
import Language.Type
import Utility.Basic

builtins :: [(Text,Type)]
builtins = [
    ("fst", TyArr (TyTuple [TyVar "a", TyVar "b"]) (TyVar "a")),
    ("snd", TyArr (TyTuple [TyVar "a", TyVar "b"]) (TyVar "b")),
    ("empty", TyArr (TyList (TyVar "a")) (TyLit "Bool")),
    ("head", TyArr (TyList (TyVar "a")) (TyVar "a")),
    ("tail", TyArr (TyList (TyVar "a")) (TyList (TyVar "a")))
  ]

opType :: Type -> Name -> Type
opType _ "<=" = TyLit "Bool"
opType t _ = t
