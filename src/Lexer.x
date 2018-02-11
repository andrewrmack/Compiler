{
-- Suppress warning from Alex-generated code
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Lexer where

import qualified Data.ByteString.Lazy.Char8 as LC
import Data.ByteString.Lex.Fractional
import Lang

}

%wrapper "posn-bytestring"

$digit   = 0-9
@decimal = $digit+
@float   = @decimal \. @decimal

tokens :-

$white+  ;
\(       { \p s -> locate p TLParen }
\)       { \p s -> locate p TRParen }
\+       { \p s -> locate p (TOp Plus) }
\-       { \p s -> locate p (TOp Minus) }
\*       { \p s -> locate p (TOp Times) }
\/       { \p s -> locate p (TOp Divide) }
\<\=     { \p s -> locate p TLte }
if       { \p s -> locate p TIf }
true     { \p s -> locate p (TBool True) }
false    { \p s -> locate p (TBool False) }
NaN      { \p s -> locate p (TFloat (0.0 / 0.0)) }
@float   { \p s -> locate p (TFloat (lexFloat p s)) }
@decimal { \p s -> locate p (TInt (lexInt p s)) }

{
lexer :: ByteString.ByteString -> [Token]
lexer s = map (\(Located _ _ t) -> t) $ alexScanTokens s

-- | Produce a Located Token from a Token and Alex's position information
locate :: AlexPosn -> Token -> Located Token
locate (AlexPn _ r c) t = Located r c t

-- | Read a Double from a ByteString (expensive, but looks unavoidable)
lexFloat :: AlexPosn -> ByteString.ByteString -> Double
lexFloat p s = case readDecimal (ByteString.toStrict s) of
                 Just (f,_) -> f
                 _ -> locatedError p $ "Can't lex float " ++ LC.unpack s

-- | Read an Int from a ByteString
lexInt :: AlexPosn -> ByteString.ByteString -> Int
lexInt p s = case LC.readInt s of
               Just (n,_) -> n
               _ -> locatedError p $ "Unable to lex integer " ++ LC.unpack s

-- | Display an error message with row and column info
locatedError :: AlexPosn -> String -> a
locatedError (AlexPn _ r c) msg = errorWithoutStackTrace locatedMsg
  where
    locatedMsg = msg ++ " at (" ++ show r ++ ":" ++ show c ++ ")"
}
