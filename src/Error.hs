module Error where

import Location (Location(..))

locatedError :: Location -> String -> a
locatedError (Location r c) msg = errorWithoutStackTrace msg'
  where
    msg' = msg ++ " at (line " ++ show r ++ ", column " ++ show c ++ ")"
