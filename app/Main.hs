module Main where

import Data.Semigroup      ((<>))
import Options.Applicative
import System.Environment  (getArgs)

import Lib

data Args = Args
    { lenFlag  :: Bool
    , leftoverArgs :: [String]
    }

parseLenFlag :: Parser Bool
parseLenFlag = switch
    ( long "length"
   <> short 'l'
   <> help "prints the lengths of the arguments")

parseLeftovers :: Parser [String]
parseLeftovers = many (strArgument (metavar "args"))

parseArgs :: Parser Args
parseArgs = Args <$> parseLenFlag <*> parseLeftovers

main :: IO ()
main = showOptions =<< execParser opts
  where
    opts = info (parseArgs <**> helper)
      ( fullDesc
     <> progDesc "Print command line arguments on separate lines"
     <> header "compiler - a compiler for CSC-312" )

showOptions :: Args -> IO ()
showOptions (Args b args) = mapM_ displayFunc args
  where
    displayFunc = if b then (putStrLn . show . length) else putStrLn
