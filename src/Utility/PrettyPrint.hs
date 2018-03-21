{-# LANGUAGE OverloadedStrings #-}
module Utility.PrettyPrint
  ( Printable(..)
  , list
  , wrap
  , parens
  , (<>)
  , (<+>)
  ) where

import Data.Text as T
import Data.Semigroup ((<>))

class Printable a where
  ppr :: a -> Text

instance Printable a => Printable [a] where
  ppr = wrap "[" "]" . list . Prelude.map ppr
  {-# INLINE ppr #-}

(<+>) :: Text -> Text -> Text
t1 <+> t2 = t1 <> " " <> t2

list :: [Text] -> Text
list = intercalate ","

wrap :: Text -> Text -> Text -> Text
wrap l r b = l <> b <> r

parens :: Text -> Text
parens = wrap "(" ")"
