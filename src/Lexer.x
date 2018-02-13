{
-- Suppress warning from Alex-generated code
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Lexer (lexer) where

import qualified Data.ByteString.Lazy.Char8 as LC
import Data.ByteString.Lex.Fractional
import Error
import Lang

}

%wrapper "posn-bytestring"

$digit   = 0-9
@decimal = $digit+
@float   = @decimal \. @decimal

tokens :-

$white+  ;
\(       { \p s -> TLParen (locate p)                }
\)       { \p s -> TRParen (locate p)                }
\+       { \p s -> TPlus   (locate p)                }
\-       { \p s -> TMinus  (locate p)                }
\*       { \p s -> TTimes  (locate p)                }
\/       { \p s -> TDivide (locate p)                }
\<\=     { \p s -> TLte    (locate p)                }
if       { \p s -> TIf     (locate p)                }
then     { \p s -> TThen   (locate p)                }
else     { \p s -> TElse   (locate p)                }
true     { \p s -> TBool   (locate p) True           }
false    { \p s -> TBool   (locate p) False          }
NaN      { \p s -> TFloat  (locate p) (0.0 / 0.0)    }
@float   { \p s -> TFloat  (locate p) (lexFloat p s) }
@decimal { \p s -> TInt    (locate p) (lexInt p s)   }

{
lexer :: ByteString.ByteString -> [Token Located]
lexer = alexScanTokens

-- | Produce a Located Token from a Token and Alex's position information
locate :: AlexPosn -> Located
locate (AlexPn _ r c) = Located r c

-- | Read a Double from a ByteString (expensive, but looks unavoidable)
lexFloat :: AlexPosn -> ByteString.ByteString -> Double
lexFloat p s = case readDecimal (ByteString.toStrict s) of
                 Just (f,_) -> f
                 _ -> locatedError (locate p) $ "Can't lex float " ++ LC.unpack s

-- | Read an Int from a ByteString
lexInt :: AlexPosn -> ByteString.ByteString -> Int
lexInt p s = case LC.readInt s of
               Just (n,_) -> n
               _ -> locatedError (locate p) $ "Unable to lex integer " ++ LC.unpack s
}
