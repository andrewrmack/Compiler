{
-- Suppress warning from Alex-generated code
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-# LANGUAGE OverloadedStrings #-}

module Lexer
  ( lexerP
  , lexer
  , Alex(..)
  , runAlex
  ) where

import Control.Monad
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.ByteString.Lex.Fractional
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding as TE

import Language.Token
import Utility.Error
import Utility.Location

}

%wrapper "monadUserState-bytestring"

$digit    = 0-9
$lower    = [a-z]
$upper    = [A-Z]
$namechar = [a-zA-Z0-9'_] @decimal  = $digit+
@float    = @decimal \. @decimal
@lname    = $lower $namechar*
@uname    = $upper $namechar*

tokens :-

$white+  ;

<ncomment> {
 \{\- { openComment }
 .    { skip }
 \-\} { closeComment }
}

<0> {
 \-\-.*\n ;
 \{\-     { openComment                               }
 \,       { token $ \(p,_,s,_) n -> TComma  (loc p)   }
 \\       { token $ \(p,_,s,_) n -> TBSlash (loc p)   }
 \(       { token $ \(p,_,s,_) n -> TLParen (loc p)   }
 \)       { token $ \(p,_,s,_) n -> TRParen (loc p)   }
 \[       { token $ \(p,_,s,_) n -> TLBrace (loc p)   }
 \]       { token $ \(p,_,s,_) n -> TRBrace (loc p)   }
 \:\:     { token $ \(p,_,s,_) n -> TDColon (loc p)   }
 \:       { token $ \(p,_,s,_) n -> TColon  (loc p)   }
 \;       { token $ \(p,_,s,_) n -> TSemi   (loc p)   }
 \+       { token $ \(p,_,s,_) n -> TPlus   (loc p)   }
 \-       { token $ \(p,_,s,_) n -> TMinus  (loc p)   }
 \*       { token $ \(p,_,s,_) n -> TTimes  (loc p)   }
 \/       { token $ \(p,_,s,_) n -> TDivide (loc p)   }
 \<\=     { token $ \(p,_,s,_) n -> TLte    (loc p)   }
 \=       { token $ \(p,_,s,_) n -> TEqual  (loc p)   }
 \-\>     { token $ \(p,_,s,_) n -> TRArrow (loc p)   }
 if       { token $ \(p,_,s,_) n -> TIf     (loc p)   }
 then     { token $ \(p,_,s,_) n -> TThen   (loc p)   }
 else     { token $ \(p,_,s,_) n -> TElse   (loc p)   }
 let      { token $ \(p,_,s,_) n -> TLet    (loc p)   }
 in       { token $ \(p,_,s,_) n -> TIn     (loc p)   }
 fun      { token $ \(p,_,s,_) n -> TFun    (loc p)   }
 fix      { token $ \(p,_,s,_) n -> TFix    (loc p)   }
 true     { token $ \(p,_,s,_) n -> TBool   (loc p) True         }
 false    { token $ \(p,_,s,_) n -> TBool   (loc p) False        }
 NaN      { token $ \(p,_,s,_) n -> TFloat  (loc p) (0.0 / 0.0)  }
 @float   { token $ \(p,_,s,_) n -> TFloat  (loc p) (lexFloat p (B.take n s)) }
 @decimal { token $ \(p,_,s,_) n -> TInt    (loc p) (lexInt p (B.take n s))   }
 @lname   { token $ \(p,_,s,_) n -> TLid    (loc p) (fromBS (B.take n s))  }
 @uname   { token $ \(p,_,s,_) n -> TUid    (loc p) (fromBS (B.take n s))  }
 .        { \(n,_,s,_) _ -> lexErrorOn n s                        }
}

{
newtype AlexUserState = AlexUserState { commentDepth :: Int }

openComment :: AlexAction Token
openComment s n = do
  alexSetStartCode ncomment
  incrementCommentDepth
  skip s n

closeComment :: AlexAction Token
closeComment s n = do
  decrementCommentDepth
  b <- isCommentEnd
  when b (alexSetStartCode 0)
  skip s n

isCommentEnd :: Alex Bool
isCommentEnd = Alex $ \s@AlexState{alex_ust=ust} ->
  Right (s, 0 == commentDepth ust)

incrementCommentDepth :: Alex ()
incrementCommentDepth = Alex $ \s@AlexState{alex_ust=ust} ->
  Right (s{alex_ust=ust{commentDepth = (commentDepth ust) + 1}}, ())

decrementCommentDepth :: Alex ()
decrementCommentDepth = Alex $ \s@AlexState{alex_ust=ust} ->
  Right (s{alex_ust=ust{commentDepth = (commentDepth ust) - 1}}, ())

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState 0

alexEOF :: Alex Token
alexEOF = do
  b <- isCommentEnd
  if b then return (TEof NoLocation) else locatedError NoLocation "Unclosed comment"

lexerP :: (Token -> Alex a) -> Alex a
lexerP = (alexMonadScan >>=)

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

lexer :: ByteString.ByteString -> [Token]
lexer bs = case lexer' bs of
             Left s -> errorWithoutStackTrace s
             Right ts -> ts

lexer' :: ByteString.ByteString -> Either String [Token]
lexer' inp = runAlex inp gather
    where
        gather = do
            t <- alexMonadScan
            case t of
              TEof _ -> return [TEof NoLocation]
              _        -> (t:) `liftM` gather

}
