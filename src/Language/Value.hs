{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Language.Value where

import Control.DeepSeq (NFData)
import Data.Text       (pack)
import GHC.Generics    (Generic)
import Language.Expression
import Utility.Basic
import Utility.PrettyPrint

data Value =
    VEmpty
  | VVar   Name
  | VInt   {-# UNPACK #-} !Int
  | VFloat {-# UNPACK #-} !Double
  | VBool  !Bool
  | VTuple [Value]
  | VList [Value]
  | VLam  Name Expr
  | VFix  Name Name Expr
  deriving (Generic)

instance NFData Value

instance Printable Value where
  -- n.b. VBools are handled differently because the object language uses
  -- lowercase for booleans, whereas Haskell uses uppercase identifiers.
  ppr VEmpty      = ""
  ppr (VInt i)    = pack $ show i
  ppr (VFloat f)  = pack $ show f
  ppr (VBool b)   = if b then "true" else "false"
  ppr (VTuple es) = parens . list $ map ppr es
  ppr (VList es)  = ppr es
  ppr _           = "Non-printable value"
  {-# INLINE ppr #-}

isFinal :: Value -> Bool
isFinal VEmpty   = True
isFinal VInt{}   = True
isFinal VBool{}  = True
isFinal VFloat{} = True
isFinal VTuple{} = True
isFinal VList{}  = True
isFinal _        = False
