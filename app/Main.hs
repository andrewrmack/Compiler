module Main where

import Data.Semigroup      ((<>))
import qualified Data.Text.IO as TIO
import Data.Version        (showVersion)
import Options.Applicative

import Interpreter
import Paths_compiler      (version)

data Args = Args { versionFlag :: Bool, input :: FilePath }

parseVersionFlag :: Parser Bool
parseVersionFlag = switch
  ( long "version"
 <> short 'v'
 <> help "Show the version number")

parseFileName :: Parser String
parseFileName = argument str (metavar "FILE")

parseArgs :: Parser Args
parseArgs = Args <$> parseVersionFlag <*> parseFileName

main :: IO ()
main = do
  (Args ver file) <- execParser opts
  if ver
  then putStrLn $ "compiler, version " ++ showVersion version
  else interpretFile file
  where
    opts = info (parseArgs <**> helper)
      ( fullDesc
     <> progDesc "Print command line arguments on separate lines"
     <> header "compiler - a compiler for CSC-312" )

interpretFile :: String -> IO ()
interpretFile file = do
  contents <- TIO.readFile file
  TIO.putStrLn $ evaluate contents
