{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Language.Value where

import Control.DeepSeq (NFData)
import Data.Text       (pack)
import GHC.Generics    (Generic)
import Utility.PrettyPrint

data Value =
    VEmpty
  | VInt   {-# UNPACK #-} !Int
  | VFloat {-# UNPACK #-} !Double
  | VBool  !Bool
  | VTuple [Value]
  | VList [Value]
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
  {-# INLINE ppr #-}
