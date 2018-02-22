{
module Parser (parse) where

import Control.Lens
import Data.Text (Text)
import Error
import Lang
import Location
}

%expect 20
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

iexp :: { Expr Location }
iexp : iexp '+'  iexp           { EOp (locate $2) Plus $1 $3                }
     | iexp '-'  iexp           { EOp (locate $2) Minus $1 $3               }
     | iexp '*'  iexp           { EOp (locate $2) Times $1 $3               }
     | iexp '/'  iexp           { EOp (locate $2) Divide $1 $3              }
     | iexp '<=' iexp           { EOp (locate $2) Lte $1 $3                 }
     | lexp                     { $1 }

lexp :: { Expr Location }
lexp : if iexp then iexp else iexp { EIf (locate $1) $2 $4 $6                  }
     | let id '=' iexp in iexp     { ELet (locate $1) ($2^?!tid) $4 $6         }
     | fun id '->' iexp            { ELam (locate $1) ($2^?!tid) $4            }
     | fix id id '->' iexp         { EFix (locate $1) ($2^?!tid) ($3^?!tid) $5 }
     | fexp                        { $1                                        }

fexp :: { Expr Location }
fexp : fexp aexp                { EApp (locate $1) $1 $2                    }
     | aexp                     { $1                                        }

aexp :: { Expr Location }
aexp : int                      { EInt (locate $1) ($1^?!tint)              }
     | float                    { EFloat (locate $1) ($1^?!tfloat)          }
     | bool                     { EBool (locate $1) ($1^?!tbool)            }
     | id                       { EVar (locate $1) ($1^?!tid)               }
     | '(' iexp ')'             { $2                                        }


{
parse :: [Token Location] -> Expr Location
parse [] = EEmpty (Location 0 0)
parse ts = parseRaw ts

parseError :: [Token Location] -> a
parseError []     = errorWithoutStackTrace "Parse error at unknown location"
parseError (t:ts) = locatedError (locate t) "Parse error"
}
