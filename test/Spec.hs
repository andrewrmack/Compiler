import System.Exit    (exitWith)
import System.Process (system)

main :: IO ()
main = do
  putStrLn "" -- Fix weird formatting
  exitcode <- system "./test/extrinsic-tests.sh"
  exitWith exitcode
