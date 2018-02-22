{-# LANGUAGE FlexibleInstances #-}
module Location where

import Control.Lens
import Lang

data Location = Location {-# UNPACK #-} !Int -- Row
                         {-# UNPACK #-} !Int -- Column

class Located a where
  locate :: a -> Location

instance Located (Token Location) where
  locate t = t^.ttag

instance Located (Expr Location) where
  locate e = e^.etag
