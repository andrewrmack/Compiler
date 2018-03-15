{-# LANGUAGE OverloadedStrings #-}
module Error
  ( locatedError
  , logWarning
  , showWarnings
  , runCompiler
  , Compiler()
  ) where

import Control.Monad.Writer
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Location (Location(..))
import System.Exit
import System.IO
import System.IO.Unsafe

type Compiler a = Writer [Text] a

data MessageType = Error | Warning

printMsgType :: MessageType -> Text
printMsgType Error = "Error"
printMsgType Warning = "Warning"

locatedError :: Location -> Text -> a
locatedError l msg = fatalError $ locatedMessage Error l msg

fatalError :: Text -> a
fatalError msg = unsafePerformIO $ do
  TIO.hPutStrLn stderr msg
  exitFailure

logWarning :: Location -> Text -> Compiler ()
logWarning l msg = tell [locatedMessage Warning l msg]

locatedMessage :: MessageType -> Location -> Text -> Text
locatedMessage t NoLocation msg = printMsgType t <> ": " <> msg <> " at unknown location"
locatedMessage t (Location r c) msg = printMsgType t <> ": " <> msg <> " at (line " <> T.pack (show r) <> ", column " <> T.pack (show c) <> ")"

showWarnings :: [Text] -> IO ()
showWarnings = mapM_ (TIO.hPutStrLn stderr)

runCompiler :: Compiler a -> (a, [Text])
runCompiler = runWriter
{-# INLINE runCompiler #-}
