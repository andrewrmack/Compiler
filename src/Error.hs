module Error where

import Lang (Located(..))

locatedError :: Located -> String -> a
locatedError (Located r c) msg = errorWithoutStackTrace msg'
  where
    msg' = msg ++ " at (line " ++ show r ++ ", column " ++ show c ++ ")"
