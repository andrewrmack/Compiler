{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Language.Expression
  ( Expr(..)
  ) where

import Control.DeepSeq (NFData)
import Data.Text       (pack)
import GHC.Generics    (Generic)

import Language.Type
import Utility.Basic
import Utility.PrettyPrint
import Utility.Location

data Expr =
    EEmpty  { eloc :: Location }
  | ECons   { eloc :: Location, eelem :: Expr, elist :: Expr }
  | ESig    { eloc :: Location, eexp :: Expr, etype :: Type }
  | EInt    { eloc :: Location, eint :: {-# UNPACK #-} !Int }
  | EFloat  { eloc :: Location, efloat ::  {-# UNPACK #-} !Double }
  | EVar    { eloc :: Location, evar :: !Name }
  | EBool   { eloc :: Location, ebool :: !Bool }
  | ETuple  { eloc :: Location, eelems :: ![Expr] }
  | EList   { eloc :: Location, eelems :: ![Expr] }
  | EApp    { eloc :: Location, eapp1 :: !Expr, eapp2 :: !Expr }
  | EOp     { eloc :: Location, eop :: !Name, eopp1 :: !Expr, eopp2 :: !Expr }
  | EIf     { eloc :: Location, eif :: !Expr, ethen :: Expr, eelse :: Expr }
  | ELet    { eloc :: Location, ename :: !Name, ebind :: Expr, ein :: Expr }
  | EFix    { eloc :: Location, efun :: !Name, evar :: !Name, ebody :: Expr }
  | ELam    { eloc :: Location, evar :: !Name, ebody :: Expr }
  deriving (Generic)

instance NFData Expr

instance Located Expr where
  locate  = eloc
  {-# INLINE locate #-}


instance Printable Expr where
  ppr (EEmpty _)          = ""
  ppr (ECons _ e1 e2)     = ppr e1 <> ":" <> ppr e2
  ppr (ESig _ e _)        = ppr e
  ppr (EVar _ n)          = n
  ppr (EInt _ n)          = pack $ show n
  ppr (EFloat _ f)        = pack $ show f
  ppr (EBool _ True)      = "true"
  ppr (EBool _ False)     = "false"
  ppr (ETuple _ es)       = parens . list $ map ppr es
  ppr (EList  _ es)       = ppr es
  ppr (EApp _ e1 e2)      = parens $ ppr e1 <+> ppr e2
  ppr (EOp _ o e1 e2)     = parens $ o <+> ppr e1 <+> ppr e2
  ppr (EIf _ e1 e2 e3)    = parens $ "if"   <+> ppr e1 <+> ppr e2 <+> ppr e3
  ppr (ELet _ n e1 e2)    = parens $ "let" <+> parens (n <+> "=" <+> ppr e1) <+> ppr e2
  ppr (EFix _ f n e)      = parens $ "fix" <+> f <+> n <+> "->" <+> ppr e
  ppr (ELam _ n e)        = parens $ "fun" <+> n <+> "->" <+> ppr e
