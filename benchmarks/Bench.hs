import Control.DeepSeq   (rnf)
import Control.Monad.Writer
import qualified Control.Exception as E
import Criterion.Main
import Data.List         (intercalate)
import qualified Data.ByteString.Lazy.Char8 as BLC
import Parser
import Interpreter

-- rawInput1 tests the elimination of whitespace by the lexer
rawInput1 = BLC.pack $ intercalate (replicate (2^16) ' ') ["23", "+", "12"]

-- rawInput2 tests parsing of deeply nested addition on the left
rawInput2 = BLC.pack $ foldl (\acc x -> acc ++ " + " ++ show x) "1" [1..2^10]

-- rawInput3 tests parsing of deeply nested addition on the right
rawInput3 = BLC.pack $ foldr (\x acc -> show x ++ " + " ++ acc) "1" [1..2^10]

-- rawInput4 is a more normal construction
rawInput4 = BLC.pack "let x = 3 in x + 2"

-- rawInput5 test lexing and evaluation of large constants
rawInput5 = BLC.pack $ show (2^5000 + 123456) ++ " + " ++ show (5^9000 - 567434532)

ast1 = parse rawInput1
ast2 = parse rawInput2
ast3 = parse rawInput3
ast4 = parse rawInput4
ast5 = parse rawInput5

main = do
  E.evaluate $ rnf rawInput1
  E.evaluate $ rnf rawInput2
  E.evaluate $ rnf rawInput3
  E.evaluate $ rnf rawInput4
  E.evaluate $ rnf rawInput5
  E.evaluate $ rnf ast1
  E.evaluate $ rnf ast2
  E.evaluate $ rnf ast3
  E.evaluate $ rnf ast4
  E.evaluate $ rnf ast5
  defaultMain [ bgroup "parse" [ bench "1" $ nf parse rawInput1
                               , bench "2" $ nf parse rawInput2
                               , bench "3" $ nf parse rawInput3
                               , bench "4" $ nf parse rawInput4
                               , bench "5" $ nf parse rawInput5
                               ]
              , bgroup "inter" [ bench "inter1" $ nf (runWriter . interpret) ast1
                               , bench "inter2" $ nf (runWriter . interpret) ast2
                               , bench "inter3" $ nf (runWriter . interpret) ast3
                               , bench "inter4" $ nf (runWriter . interpret) ast4
                               , bench "inter5" $ nf (runWriter . interpret) ast5
                               ]
              , bgroup "eval"  [ bench "eval1" $ nf (runWriter . evaluate) rawInput1
                               , bench "eval2" $ nf (runWriter . evaluate) rawInput2
                               , bench "eval3" $ nf (runWriter . evaluate) rawInput3
                               , bench "eval4" $ nf (runWriter . evaluate) rawInput4
                               , bench "eval5" $ nf (runWriter . evaluate) rawInput5
                               ]
              ]
