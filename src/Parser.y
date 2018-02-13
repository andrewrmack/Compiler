{
module Parser (parse) where

import Error
import Lang
}

%name parseRaw
%tokentype { Token Located }
%error { parseError }

%nonassoc '<='
%left '+' '-'
%left '*' '/'

%token
      '('     { TLParen _   }
      ')'     { TRParen _   }
      '<='    { TLte    _   }
      if      { TIf     _   }
      then    { TThen   _   }
      else    { TElse   _   }
      '+'     { TPlus   _   }
      '-'     { TMinus  _   }
      '*'     { TTimes  _   }
      '/'     { TDivide _   }
      bool    { TBool   _ _ }
      int     { TInt    _ _ }
      float   { TFloat  _ _ }

%%

exp  :: { Expr Located }
exp  : if exp then exp else exp { EIf (locate $1) $2 $4 $6         }
     | exp1                     { $1                               }

exp1 :: { Expr Located }
exp1 : exp1 '+'  exp1           { EOp (locate $2) Plus $1 $3       }
     | exp1 '-'  exp1           { EOp (locate $2) Minus $1 $3      }
     | exp1 '*'  exp1           { EOp (locate $2) Times $1 $3      }
     | exp1 '/'  exp1           { EOp (locate $2) Divide $1 $3     }
     | exp1 '<=' exp1           { EOp (locate $2) Lte $1 $3        }
     | int                      { EInt (locate $1) (getInt $1)     }
     | float                    { EFloat (locate $1) (getFloat $1) }
     | bool                     { EBool (locate $1) (getBool $1)   }
     | '(' exp ')'              { $2                               }


{
-- I hate that this is necessary, but there doesn't seem to be a great
-- solution to this in Haskell.
locate :: Token Located -> Located
locate (TLParen l)  = l
locate (TRParen l)  = l
locate (TLte l)     = l
locate (TIf l)      = l
locate (TThen l)    = l
locate (TElse l)    = l
locate (TPlus l)    = l
locate (TMinus l)   = l
locate (TTimes l)   = l
locate (TDivide l)  = l
locate (TBool l _)  = l
locate (TInt l _)   = l
locate (TFloat l _) = l

parse :: [Token Located] -> Expr Located
parse [] = EEmpty
parse ts = parseRaw ts

getInt :: Token a -> Int
getInt (TInt _ n) = n

getFloat :: Token a -> Double
getFloat (TFloat _ f) = f

getBool :: Token a -> Bool
getBool (TBool _ b) = b

parseError :: [Token Located] -> a
parseError []     = errorWithoutStackTrace "Parse error at unknown location"
parseError (t:ts) = locatedError (locate t) "Parse error"
}
