module Main where

import Data.Maybe          (listToMaybe, fromMaybe)
import Data.Semigroup      ((<>))
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as BL
import Data.Version        (showVersion)
import Options.Applicative

import Error
import Lang
import Lexer
import Parser
import Interpreter
import Paths_compiler      (version)

data Mode = Default | LexDump | ParseDump
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
  ( long "lex"
 <> help "print the result of lexing to standard output" )

parseParserFlag :: Parser Mode
parseParserFlag = flag Default ParseDump
  ( long "parse"
 <> help "print the result of parsing to standard output" )

parseMode :: Parser Mode
parseMode = parseLexerFlag <|> parseParserFlag

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
         LexDump -> lexFile (fromMaybe fileError (listToMaybe filepath))
         ParseDump -> parseFile (fromMaybe fileError (listToMaybe filepath))
  where
    fileError = errorWithoutStackTrace "No file given"
    opts = info (parseArgs <**> helper)
      ( fullDesc
     <> progDesc "Print command line arguments on separate lines"
     <> header "compiler - a compiler for CSC-312" )

lexFile :: String -> IO ()
lexFile file = do
  contents <- BL.readFile file
  TIO.putStrLn $ ppTokenList (lexer contents)

parseFile :: String -> IO ()
parseFile file = do
  contents <- BL.readFile file
  TIO.putStrLn $ ppExpr (parse (lexer contents))

interpretFile :: String -> IO ()
interpretFile file = do
  contents <- BL.readFile file
  let evaled   = evaluate contents
  let warnings = getWarnings evaled
  let results  = runCompiler evaled
  showWarnings warnings
  TIO.putStrLn results
