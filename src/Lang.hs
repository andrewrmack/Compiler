{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module Lang
  ( Token(..)
  , Op(..)
  , Expr(..)
  , Located(..)
  , ppOp
  , ppTokenList
  , ppExpr
  ) where

import Control.DeepSeq          (NFData)
import Data.Text                (Text)
import qualified Data.Text as T
import GHC.Generics             (Generic)

data Op = Plus | Minus | Times | Divide deriving (Generic)

data Token =
    TLParen
  | TRParen
  | TLte
  | TIf
  | TOp     !Op
  | TBool   !Bool
  | TInt    {-# UNPACK #-} !Int
  | TFloat  {-# UNPACK #-} !Double
  deriving (Generic)

instance NFData Op
instance NFData Token

-- n.b. No strictness in branches of EIf to save work.
data Expr =
    EInt   {-# UNPACK #-}!Int
  | EFloat {-# UNPACK #-} !Double
  | EBool  !Bool
  | EOp    !Op !Expr !Expr
  | ELte   !Expr !Expr
  | EIf    !Expr Expr Expr
  deriving (Generic)

instance NFData Expr

data Located a = Located {-# UNPACK #-} !Int -- Row
                         {-# UNPACK #-} !Int -- Column
                         a

ppExpr :: Expr -> Text
ppExpr (EInt n)       = T.pack $ show n
ppExpr (EFloat f)     = T.pack $ show f
ppExpr (EBool True)   = "true"
ppExpr (EBool False)  = "false"
ppExpr (EOp o e1 e2)  = T.concat ["(", ppOp o, " ", ppExpr e1, " ", ppExpr e2, ")"]
ppExpr (ELte e1 e2)   = T.concat ["(<= ", ppExpr e1, " ", ppExpr e2, ")"]
ppExpr (EIf e1 e2 e3) = T.concat ["(if ", ppExpr e1, " ", ppExpr e2, " ", ppExpr e3, ")"]

ppOp :: Op -> Text
ppOp Plus   = "+"
ppOp Minus  = "-"
ppOp Times  = "*"
ppOp Divide = "/"

ppToken :: Token -> Text
ppToken TLParen       = "("
ppToken TRParen       = ")"
ppToken TLte          = "<="
ppToken TIf           = "if"
ppToken (TOp o)       = ppOp o
ppToken (TBool True)  = "true"
ppToken (TBool False) = "false"
ppToken (TInt n)      = T.pack $ show n
ppToken (TFloat f)    = T.pack $ show f

ppTokenList :: [Token] -> Text
ppTokenList ts = T.concat ["[", T.intercalate ", " (map ppToken ts), "]"]
