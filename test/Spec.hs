import System.Exit    (exitWith)
import System.Process (system)

main :: IO ()
main = do
  exitcode <- system "./test/extrinsic-tests.sh"
  exitWith exitcode
