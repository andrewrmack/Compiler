{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Error
  ( locatedError
  , logWarning
  , getWarnings
  , showWarnings
  , runCompiler
  , Compiler()
  , CompilerState()
  ) where

import Control.Lens
import Control.Monad.State
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Location (Location(..))
import System.Exit
import System.IO
import System.IO.Unsafe

type Compiler a = State CompilerState a

data MessageType = Error | Warning

printMsgType :: MessageType -> Text
printMsgType Error = "Error"
printMsgType Warning = "Warning"

newtype CompilerState = CompilerState {
    _warnings :: [Text]
   }

emptyState :: CompilerState
emptyState = CompilerState []

makeLenses ''CompilerState

locatedError :: Location -> Text -> a
locatedError l msg = fatalError $ locatedMessage Error l msg

fatalError :: Text -> a
fatalError msg = unsafePerformIO $ do
  TIO.hPutStrLn stderr msg
  exitFailure

logWarning :: Location -> Text -> Compiler ()
logWarning l msg = modify (warnings %~ (locatedMessage Warning l msg :))

locatedMessage :: MessageType -> Location -> Text -> Text
locatedMessage t NoLocation msg = printMsgType t <> ": " <> msg <> " at unknown location"
locatedMessage t (Location r c) msg = printMsgType t <> ": " <> msg <> " at (line " <> T.pack (show r) <> ", column " <> T.pack (show c) <> ")"

getWarnings :: Compiler a -> [Text]
getWarnings c = reverse . _warnings $ execState c emptyState

showWarnings :: [Text] -> IO ()
showWarnings = mapM_ (TIO.hPutStrLn stderr)

runCompiler :: Compiler a -> a
runCompiler c = evalState c emptyState
