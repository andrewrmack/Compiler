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
import Typechecker
import Interpreter
import Paths_compiler      (version)

data Mode = Default | LexDump | ParseDump | TypeDump
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

parseMode :: Parser Mode
parseMode = parseLexerFlag <|> parseParserFlag <|> parseTypeFlag

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
  TIO.putStrLn $ ppTokenList (filter notEof (lexer contents))
  where
    notEof (TEof _) = False  -- Remove EOFs so I don't have to remake test suite
    notEof _ = True

parseFile :: String -> IO ()
parseFile file = do
  contents <- BL.readFile file
  TIO.putStrLn $ ppExpr (parse contents)

typeFile :: String -> IO ()
typeFile file = do
  contents <- BL.readFile file
  TIO.putStrLn $ ppType (getType (parse contents))

interpretFile :: String -> IO ()
interpretFile file = do
  contents <- BL.readFile file
  let evaled = evaluate contents
  let (results, warnings) = runCompiler evaled
  showWarnings warnings
  TIO.putStrLn results
