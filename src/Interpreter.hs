{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Interpreter (evaluate, interpret) where

import Control.DeepSeq
import Data.ByteString.Lazy     (ByteString)
import Data.Text                (Text)
import qualified Data.Text as T
import GHC.Generics             (Generic)
import Lexer
import Parser
import Lang

data Value =
    VInt   {-# UNPACK #-} !Int
  | VFloat {-# UNPACK #-} !Double
  | VBool  !Bool
  deriving (Generic)

instance NFData Value

-- n.b. VBools are handled differently because the object language uses
-- lowercase for booleans, whereas Haskell uses uppercase identifiers.
showValue :: Value -> Text
showValue (VInt i)   = T.pack $ show i
showValue (VFloat f) = T.pack $ show f
showValue (VBool b)  = if b then "true" else "false"
{-# INLINE showValue #-}

evaluate :: ByteString -> Text
evaluate = showValue . interpret . parse . lexer
{-# INLINE evaluate #-}

interpret :: Expr -> Value
interpret (EInt n) = VInt n
interpret (EBool b) = VBool b
interpret (EFloat f) = VFloat f
interpret (EIf e1 e2 e3) = if b1 then interpret e2 else interpret e3
  where
    b1 = case interpret e1 of
           (VBool b) -> b
           _ -> errorWithoutStackTrace "Cannot evaluate 'if' with non-boolean condition"
interpret (ELte e1 e2) =
  case (interpret e1, interpret e2) of
    (VFloat f1, VFloat f2) -> VBool $ f1 <= f2
    (VFloat f1, VInt n2)   -> VBool $ f1 <= fromIntegral n2
    (VInt n1, VFloat f2)   -> VBool $ fromIntegral n1 <= f2
    (VInt n1, VInt n2)     -> VBool $ n1 <= n2
    _ -> errorWithoutStackTrace "Cannot compare non number"
interpret (EOp op e1 e2) =
  case (interpret e1, interpret e2) of
    (VFloat f1, VFloat f2) -> VFloat $ floatOp op f1 f2
    (VFloat f1, VInt n2)   -> VFloat $ floatOp op f1 (fromIntegral n2)
    (VInt n1, VFloat f2)   -> VFloat $ floatOp op (fromIntegral n1) f2
    (VInt n1, VInt n2)     -> VInt $ intOp op n1 n2
    _ -> errorWithoutStackTrace "Cannot perform arithmetic operation on non number"

floatOp :: Op -> (Double -> Double -> Double)
floatOp Plus = (+)
floatOp Minus = (-)
floatOp Times = (*)
floatOp Divide = (/)
{-# INLINE floatOp #-}

intOp :: Op -> (Int -> Int -> Int)
intOp Plus = (+)
intOp Minus = (-)
intOp Times = (*)
intOp Divide = \n m -> if n == 0 then zeroError else n `quot` m
  where zeroError = errorWithoutStackTrace "Error: Divide by zero"
{-# INLINE intOp #-}
