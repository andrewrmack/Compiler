{-# LANGUAGE TemplateHaskell #-}
module Error
  ( locatedError
  , logWarning
  , getWarnings
  , showWarnings
  , runCompiler
  , Compiler()
  )where

import Control.Lens
import Control.Monad.State
import Location (Location(..))
import System.IO

type Compiler a = State CompilerState a

newtype CompilerState = CompilerState {
    _warnings :: [String]
   }

emptyState :: CompilerState
emptyState = CompilerState []

makeLenses ''CompilerState

locatedError :: Location -> String -> a
locatedError l msg = errorWithoutStackTrace $ locatedMessage l msg

logWarning :: Location -> String -> Compiler ()
logWarning l msg = modify (warnings %~ (locatedMessage l msg :))

locatedMessage :: Location -> String -> String
locatedMessage (Location r c) msg =
  msg ++ " at (line " ++ show r ++ ", column " ++ show c ++ ")"

getWarnings :: Compiler a -> [String]
getWarnings c = _warnings $ execState c emptyState

showWarnings :: [String] -> IO ()
showWarnings = mapM_ (hPutStrLn stderr)

runCompiler :: Compiler a -> a
runCompiler c = evalState c emptyState
