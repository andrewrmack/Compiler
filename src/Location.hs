{-# LANGUAGE FlexibleInstances #-}
module Location where

import Lang

data Location = Location {-# UNPACK #-} !Int -- Row
                         {-# UNPACK #-} !Int -- Column

class Located a where
  locate :: a -> Location

instance Located (Token Location) where
  locate (TLParen l)   = l
  locate (TRParen l)   = l
  locate (TLte    l)   = l
  locate (TEqual  l)   = l
  locate (TFun    l)   = l
  locate (TFix    l)   = l
  locate (TRArrow l)   = l
  locate (TIf     l)   = l
  locate (TThen   l)   = l
  locate (TElse   l)   = l
  locate (TLet    l)   = l
  locate (TIn     l)   = l
  locate (TPlus   l)   = l
  locate (TMinus  l)   = l
  locate (TTimes  l)   = l
  locate (TDivide l)   = l
  locate (TBool   l _) = l
  locate (TId     l _) = l
  locate (TInt    l _) = l
  locate (TFloat  l _) = l

instance Located (Expr Location) where
  locate EEmpty = errorWithoutStackTrace "Can't locate empty expression"
  locate (EInt l _) = l
  locate (EFloat l _) = l
  locate (EVar l _) = l
  locate (EBool l _) = l
  locate (EApp l _ _) = l
  locate (EOp l _ _ _) = l
  locate (EIf l _ _ _) = l
  locate (ELet l _ _ _) = l
  locate (EFix l _ _ _) = l
  locate (ELam l _ _) = l
