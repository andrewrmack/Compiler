{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Token
  ( Token(..)
  ) where

import Data.Text
import GHC.Generics (Generic)

import Utility.Basic
import Utility.PrettyPrint
import Utility.Location

data Token =
    TLParen { tloc :: Location }
  | TEof    { tloc :: Location }
  | TRParen { tloc :: Location }
  | TBSlash { tloc :: Location }
  | TLte    { tloc :: Location }
  | TComma  { tloc :: Location }
  | TDColon { tloc :: Location }
  | TColon  { tloc :: Location }
  | TSemi   { tloc :: Location }
  | TLBrace { tloc :: Location }
  | TRBrace { tloc :: Location }
  | TEqual  { tloc :: Location }
  | TFix    { tloc :: Location }
  | TRArrow { tloc :: Location }
  | TIf     { tloc :: Location }
  | TThen   { tloc :: Location }
  | TElse   { tloc :: Location }
  | TLet    { tloc :: Location }
  | TIn     { tloc :: Location }
  | TPlus   { tloc :: Location }
  | TMinus  { tloc :: Location }
  | TTimes  { tloc :: Location }
  | TDivide { tloc :: Location }
  | TBool   { tloc :: Location, tbool :: !Bool }
  | TLid    { tloc :: Location, tid :: !Name }
  | TUid    { tloc :: Location, tid :: !Name }
  | TInt    { tloc :: Location, tint ::{-# UNPACK #-} !Int }
  | TFloat  { tloc :: Location, tfloat :: {-# UNPACK #-} !Double }
  deriving (Generic)

instance Located Token where
  locate = tloc
  {-# INLINE locate #-}

instance Printable Token where
  ppr (TLParen _)     = "("
  ppr (TRParen _)     = ")"
  ppr (TBSlash _)     = "\\"
  ppr (TLte _)        = "<="
  ppr (TComma _)      = ","
  ppr (TDColon _)     = "::"
  ppr (TColon _)      = ":"
  ppr (TSemi _)       = ";"
  ppr (TLBrace _)     = "["
  ppr (TRBrace _)     = "]"
  ppr (TEqual _)      = "="
  ppr (TRArrow _)     = "->"
  ppr (TIf _)         = "if"
  ppr (TThen _)       = "then"
  ppr (TElse _)       = "else"
  ppr (TLet _)        = "let"
  ppr (TIn _)         = "in"
  ppr (TFix _)        = "fix"
  ppr (TPlus _)       = "+"
  ppr (TMinus _)      = "-"
  ppr (TTimes _)      = "*"
  ppr (TDivide _)     = "/"
  ppr (TBool _ True)  = "true"
  ppr (TBool _ False) = "false"
  ppr (TLid _ t)      = t
  ppr (TUid _ t)      = t
  ppr (TInt _ n)      = pack $ show n
  ppr (TFloat _ f)    = pack $ show f
  ppr (TEof _)        = ""
