module Main where

import Control.Exception.Base (ErrorCall(..))
import Control.Monad.IO.Class
import Data.Maybe          (listToMaybe, fromMaybe)
import Data.Semigroup      ((<>))
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Version        (showVersion)
import Options.Applicative
import System.Console.Haskeline
import System.IO

import Lexer
import Parser
import Typechecker
import Interpreter
import Utility.Error
import Utility.PrettyPrint

import Paths_compiler      (version)

data Mode = Default | LexDump | ParseDump | TypeDump | Interactive
data Args = Args
  { versionFlag :: Bool
  , input :: [FilePath]
  , mode :: Mode
  }

parseVersionFlag :: Parser Bool
parseVersionFlag = switch
  ( long "version"
 <> short 'v'
 <> help "Show the version number")

parseLexerFlag :: Parser Mode
parseLexerFlag = flag' LexDump
  ( long "ddump-lex"
 <> help "print the result of lexing to standard output" )

parseParserFlag :: Parser Mode
parseParserFlag = flag' ParseDump
  ( long "ddump-parse"
 <> help "print the result of parsing to standard output" )

parseTypeFlag :: Parser Mode
parseTypeFlag = flag Default TypeDump
  ( long "ddump-typecheck"
 <> help "print the result of typechecking to standard output" )

parseInterFlag :: Parser Mode
parseInterFlag = flag' Interactive
  ( long "interactive"
 <> short 'i'
 <> help "Run and interactive REPL" )


parseMode :: Parser Mode
parseMode = parseInterFlag <|> parseLexerFlag <|> parseParserFlag <|> parseTypeFlag

parseFileName :: Parser [String]
parseFileName = many $ argument str (metavar "FILE")

parseArgs :: Parser Args
parseArgs = Args <$> parseVersionFlag <*> parseFileName <*> parseMode

main :: IO ()
main = do
  (Args ver filepath compileMode) <- execParser opts
  if ver
  then putStrLn $ "compiler, version " ++ showVersion version
  else case compileMode of
         Default -> interpretFile (fromMaybe fileError (listToMaybe filepath))
         Interactive -> interactive
         LexDump -> lexFile (fromMaybe fileError (listToMaybe filepath))
         ParseDump -> parseFile (fromMaybe fileError (listToMaybe filepath))
         TypeDump -> typeFile (fromMaybe fileError (listToMaybe filepath))
  where
    fileError = errorWithoutStackTrace "No file given"
    opts = info (parseArgs <**> helper)
      ( fullDesc
     <> progDesc "Print command line arguments on separate lines"
     <> header "compiler - a compiler for CSC-312" )

lexFile :: String -> IO ()
lexFile file =
  do
  contents <- BL.readFile file
  TIO.putStrLn $ ppr (lexer contents)

parseFile :: String -> IO ()
parseFile file = do
  contents <- BL.readFile file
  TIO.putStrLn $ ppr (parse contents)

typeFile :: String -> IO ()
typeFile file = do
  contents <- BL.readFile file
  TIO.putStrLn $ ppr (getType (parse contents))

interactive :: IO ()
interactive = runInputT defaultSettings (withInterrupt repl)
  where
    repl :: InputT IO ()
    repl = handle (\Interrupt -> outputStrLn "Interrupted." >> repl)
             loop
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine ">>> "
      case minput of
        Nothing -> return ()
        Just sinput -> do
          liftIO $ doEval (BLU.fromString sinput) `catch` \(ErrorCall s) -> hPutStrLn stderr s
          loop

doEval :: BL.ByteString -> IO ()
doEval contents = do
  let evaled = evaluate (typecheck (parse contents))
  let (results, warnings) = runCompiler evaled
  showWarnings warnings
  TIO.putStrLn results

interpretFile :: String -> IO ()
interpretFile file = do
  contents <- BL.readFile file
  doEval contents
