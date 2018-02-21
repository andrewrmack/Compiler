{
module Parser (parse) where

import Data.Text (Text)
import Error
import Lang
import Location
}

%name parseRaw
%tokentype { Token Location }
%error { parseError }

%nonassoc '<='
%left '+' '-'
%left '*' '/'

%token
      '('     { TLParen _   }
      ')'     { TRParen _   }
      '<='    { TLte    _   }
      '='     { TEqual  _   }
      '->'    { TRArrow _   }
      if      { TIf     _   }
      then    { TThen   _   }
      else    { TElse   _   }
      let     { TLet    _   }
      in      { TIn     _   }
      fun     { TFun    _   }
      fix     { TFix    _   }
      '+'     { TPlus   _   }
      '-'     { TMinus  _   }
      '*'     { TTimes  _   }
      '/'     { TDivide _   }
      bool    { TBool   _ _ }
      id      { TId     _ _ }
      int     { TInt    _ _ }
      float   { TFloat  _ _ }

%%

exp  :: { Expr Location }
exp  : if exp then exp else exp { EIf (locate $1) $2 $4 $6                  }
     | let id '=' exp in exp    { ELet (locate $1) (getId $2) $4 $6         }
     | fun id '->' exp          { ELam (locate $1) (getId $2) $4            }
     | fix id id '->' exp       { EFix (locate $1) (getId $2) (getId $3) $5 }
     | fexp                     { $1                                        }

fexp :: { Expr Location }
fexp : fexp exp1                { EApp (locate $1) $1 $2                    }
     | exp1                     { $1                                        }

exp1 :: { Expr Location }
exp1 : exp1 '+'  exp1           { EOp (locate $2) Plus $1 $3                }
     | exp1 '-'  exp1           { EOp (locate $2) Minus $1 $3               }
     | exp1 '*'  exp1           { EOp (locate $2) Times $1 $3               }
     | exp1 '/'  exp1           { EOp (locate $2) Divide $1 $3              }
     | exp1 '<=' exp1           { EOp (locate $2) Lte $1 $3                 }
     | int                      { EInt (locate $1) (getInt $1)              }
     | float                    { EFloat (locate $1) (getFloat $1)          }
     | bool                     { EBool (locate $1) (getBool $1)            }
     | id                       { EVar (locate $1) (getId $1)               }
     | '(' exp ')'              { $2                                        }


{
parse :: [Token Location] -> Expr Location
parse [] = EEmpty
parse ts = parseRaw ts

getInt :: Token a -> Int
getInt (TInt _ n) = n

getFloat :: Token a -> Double
getFloat (TFloat _ f) = f

getBool :: Token a -> Bool
getBool (TBool _ b) = b

getId :: Token a -> Text
getId (TId _ n) = n

parseError :: [Token Location] -> a
parseError []     = errorWithoutStackTrace "Parse error at unknown location"
parseError (t:ts) = locatedError (locate t) "Parse error"
}
