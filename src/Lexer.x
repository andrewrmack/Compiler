{
-- Suppress warning from Alex-generated code
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# LANGUAGE OverloadedStrings #-}

module Lexer (lexer) where

import qualified Data.ByteString.Lazy.Char8 as LC
import Data.ByteString.Lex.Fractional
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding as TE
import Error
import Lang
import Location

}

%wrapper "posn-bytestring"

$digit    = 0-9
$lower    = [a-z]
$upper    = [A-Z]
$namechar = [a-zA-Z0-9'_] @decimal  = $digit+
@float    = @decimal \. @decimal
@lname    = $lower $namechar*
@uname    = $upper $namechar*

tokens :-

$white+  ;
\,       { \p s -> TComma  (loc p)                 }
\(       { \p s -> TLParen (loc p)                 }
\)       { \p s -> TRParen (loc p)                 }
\[       { \p s -> TLBrace (loc p)                 }
\]       { \p s -> TRBrace (loc p)                 }
\:\:     { \p s -> TDColon (loc p)                 }
\:       { \p s -> TColon  (loc p)                 }
\+       { \p s -> TPlus   (loc p)                 }
\-       { \p s -> TMinus  (loc p)                 }
\*       { \p s -> TTimes  (loc p)                 }
\/       { \p s -> TDivide (loc p)                 }
\<\=     { \p s -> TLte    (loc p)                 }
\=       { \p s -> TEqual  (loc p)                 }
\-\>     { \p s -> TRArrow (loc p)                 }
if       { \p s -> TIf     (loc p)                 }
then     { \p s -> TThen   (loc p)                 }
else     { \p s -> TElse   (loc p)                 }
let      { \p s -> TLet    (loc p)                 }
in       { \p s -> TIn     (loc p)                 }
fun      { \p s -> TFun    (loc p)                 }
fix      { \p s -> TFix    (loc p)                 }
true     { \p s -> TBool   (loc p) True            }
false    { \p s -> TBool   (loc p) False           }
NaN      { \p s -> TFloat  (loc p) (0.0 / 0.0)     }
@float   { \p s -> TFloat  (loc p) (lexFloat p s)  }
@decimal { \p s -> TInt    (loc p) (lexInt p s)    }
@lname   { \p s -> TLid    (loc p) (fromBS s)      }
@uname   { \p s -> TUid    (loc p) (fromBS s)      }
.        { \p s -> lexErrorOn p s                  }

{
lexer :: ByteString.ByteString -> [Token Location]
lexer = alexScanTokens

lexErrorOn :: AlexPosn -> ByteString.ByteString -> a
lexErrorOn p s = locatedError (loc p) $ "Unexpected character " <> fromBS s

-- | Produce a Located Token from a Token and Alex's position information
loc :: AlexPosn -> Location
loc (AlexPn _ r c) = Location r c

-- | Read a Double from a ByteString (expensive, but looks unavoidable)
lexFloat :: AlexPosn -> ByteString.ByteString -> Double
lexFloat p s = case readDecimal (ByteString.toStrict s) of
                 Just (f,_) -> f
                 _ -> locatedError (loc p) $ "Can't lex float " <> fromBS s

-- | Read an Int from a ByteString
lexInt :: AlexPosn -> ByteString.ByteString -> Int
lexInt p s = case LC.readInt s of
               Just (n,_) -> n
               _ -> locatedError (loc p) $ "Unable to lex integer " <> fromBS s

fromBS :: ByteString.ByteString -> Text
fromBS = TE.decodeUtf8 . ByteString.toStrict
}
