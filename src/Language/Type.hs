{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Type
  ( Type(..)
  ) where

import Control.DeepSeq (NFData)
import Data.Text       (pack)
import GHC.Generics    (Generic)

import Utility.Basic
import Utility.PrettyPrint
import Utility.Location

data Type =
    TyLit Name
  | TyVar Name
  | TyGenSym {-# UNPACK #-} !Int
  | TyTuple [Type]
  | TyList Type
  | TyArr Type Type
  deriving (Generic,Eq)

instance NFData Type

instance Located Type where
  locate = const NoLocation
  {-# INLINE locate #-}

instance Printable Type where
  ppr (TyLit t) = t
  ppr (TyVar t) = t
  ppr (TyGenSym i) = "t" <> pack (show i)
  ppr (TyTuple ts) = parens . list $ map ppr ts
  ppr (TyList t) = "[" <> ppr t <> "]"
  ppr (TyArr t1@TyArr{} t2) = parens (ppr t1) <+> "->" <+> ppr t2
  ppr (TyArr t1 t2) = ppr t1 <+> "->" <+> ppr t2
