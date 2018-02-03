import Control.DeepSeq   (rnf)
import qualified Control.Exception as E
import Criterion.Main
import Data.List         (intercalate)
import qualified Data.Text as T
import Lexer
import Parser
import Interpreter

-- rawInput1 tests the elimination of whitespace by the lexer
rawInput1 = T.pack $ intercalate (replicate (2^16) ' ') ["(", "+", "23", "12", ")"]

-- rawInput2 tests parsing of deeply nested addition on the left
rawInput2 = T.pack $ foldl (\acc x -> "(+ " ++ acc ++ " " ++ show x ++ ")") "1" [1..2^10]

-- rawInput3 tests parsing of deeply nested addition on the right
rawInput3 = T.pack $ foldr (\x acc -> "(+ " ++ show x ++ " " ++ acc ++ ")") "1" [1..2^10]

-- rawInput4 is a more normal construction
rawInput4 = T.pack "(+ (+ 1 2) (+ 3 4))"

-- rawInput5 test lexing and evaluation of large constants
rawInput5 = T.pack $ "(+ " ++ show (2^5000 + 123456) ++ " " ++ show (5^9000 - 567434532) ++ ")"

tokens1 = lexer rawInput1
tokens2 = lexer rawInput2
tokens3 = lexer rawInput3
tokens4 = lexer rawInput4
tokens5 = lexer rawInput5

ast1 = parse tokens1
ast2 = parse tokens2
ast3 = parse tokens3
ast4 = parse tokens4
ast5 = parse tokens5

main = do
  E.evaluate $ rnf rawInput1
  E.evaluate $ rnf rawInput2
  E.evaluate $ rnf rawInput3
  E.evaluate $ rnf rawInput4
  E.evaluate $ rnf rawInput5
  E.evaluate $ rnf tokens1
  E.evaluate $ rnf tokens2
  E.evaluate $ rnf tokens3
  E.evaluate $ rnf tokens4
  E.evaluate $ rnf tokens5
  E.evaluate $ rnf ast1
  E.evaluate $ rnf ast2
  E.evaluate $ rnf ast3
  E.evaluate $ rnf ast4
  E.evaluate $ rnf ast5
  defaultMain [ bgroup "lexer" [ bench "lexer1" $ nf lexer rawInput1
                               , bench "lexer2" $ nf lexer rawInput2
                               , bench "lexer3" $ nf lexer rawInput3
                               , bench "lexer4" $ nf lexer rawInput4
                               , bench "lexer5" $ nf lexer rawInput5
                               ]
              , bgroup "parse" [ bench "parse1" $ nf parse tokens1
                               , bench "parse2" $ nf parse tokens2
                               , bench "parse3" $ nf parse tokens3
                               --, bench "parse4" $ nf parse tokens4
                               , bench "parse5" $ nf parse tokens5
                               ]
              , bgroup "inter" [ bench "eval1" $ nf interpret ast1
                               , bench "eval2" $ nf interpret ast2
                               , bench "eval3" $ nf interpret ast3
                               --, bench "eval4" $ nf interpret ast4
                               , bench "eval5" $ nf interpret ast5
                               ]
              , bgroup "eval"  [ bench "eval1" $ nf evaluate rawInput1
                               , bench "eval2" $ nf evaluate rawInput2
                               , bench "eval3" $ nf evaluate rawInput3
                               --, bench "eval4" $ nf evaluate rawInput4
                               , bench "eval5" $ nf evaluate rawInput5
                               ]
              ]
