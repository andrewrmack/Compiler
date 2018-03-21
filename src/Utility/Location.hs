{-# LANGUAGE DeriveGeneric #-}
module Utility.Location
  ( Location(..)
  , Located(..)
  ) where

import Control.DeepSeq (NFData)
import GHC.Generics    (Generic)

data Location = Location {-# UNPACK #-} !Int -- Row
                         {-# UNPACK #-} !Int -- Column
              | NoLocation
              deriving Generic

instance NFData Location

class Located a where
  locate :: a -> Location
