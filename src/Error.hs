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
import System.Exit
import System.IO
import System.IO.Unsafe

type Compiler a = State CompilerState a

data MessageType = Error | Warning deriving Show

newtype CompilerState = CompilerState {
    _warnings :: [String]
   }

emptyState :: CompilerState
emptyState = CompilerState []

makeLenses ''CompilerState

locatedError :: Location -> String -> a
locatedError l msg = fatalError $ locatedMessage Error l msg

fatalError :: String -> a
fatalError msg = unsafePerformIO $ do
  hPutStrLn stderr msg
  exitFailure

logWarning :: Location -> String -> Compiler ()
logWarning l msg = modify (warnings %~ (locatedMessage Warning l msg :))

locatedMessage :: MessageType -> Location -> String -> String
locatedMessage t (Location r c) msg =
  show t ++ ": " ++ msg ++ " at (line " ++ show r ++ ", column " ++ show c ++ ")"

getWarnings :: Compiler a -> [String]
getWarnings c = reverse . _warnings $ execState c emptyState

showWarnings :: [String] -> IO ()
showWarnings = mapM_ (hPutStrLn stderr)

runCompiler :: Compiler a -> a
runCompiler c = evalState c emptyState
