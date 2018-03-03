{
{-# LANGUAGE OverloadedStrings #-}
module Parser (parse) where

import Control.Lens
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
%right ':'

%token
      '('     { TLParen _   }
      ')'     { TRParen _   }
      '['     { TLBrace _   }
      ']'     { TRBrace _   }
      '::'    { TDColon _   }
      ':'     { TColon  _   }
      ','     { TComma  _   }
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
      lid     { TLid    _ _ }
      uid     { TUid    _ _ }
      int     { TInt    _ _ }
      float   { TFloat  _ _ }

%%

exp  :: { Expr Location }
exp  : iexp '::' type     { ESig (locate $1) $1 $3       }
     | iexp               { $1                           }

iexp :: { Expr Location }
iexp : iexp '+'  iexp     { EOp (locate $2) Plus $1 $3   }
     | iexp '-'  iexp     { EOp (locate $2) Minus $1 $3  }
     | iexp '*'  iexp     { EOp (locate $2) Times $1 $3  }
     | iexp '/'  iexp     { EOp (locate $2) Divide $1 $3 }
     | iexp '<=' iexp     { EOp (locate $2) Lte $1 $3    }
     | iexp ':'  iexp     { ECons (locate $2) $1 $3      }
     | lexp               { $1                           }

lexp :: { Expr Location }
lexp : if exp then exp else exp    { EIf (locate $1) $2 $4 $6                  }
     | let lid '=' exp in exp      { ELet (locate $1) ($2^?!tid) $4 $6         }
     | fun lid '->' exp            { ELam (locate $1) ($2^?!tid) $4            }
     | fix lid lid '->' exp        { EFix (locate $1) ($2^?!tid) ($3^?!tid) $5 }
     | fexp                        { $1                                        }

fexp :: { Expr Location }
fexp : fexp aexp          { EApp (locate $1) $1 $2 }
     | aexp               { $1                     }

aexp :: { Expr Location }
aexp : int                   { EInt (locate $1) ($1^?!tint)         }
     | float                 { EFloat (locate $1) ($1^?!tfloat)     }
     | bool                  { EBool (locate $1) ($1^?!tbool)       }
     | lid                   { EVar (locate $1) ($1^?!tid)          }
     | '(' exp ',' exps ')'  { ETuple (locate $1) ($2 : reverse $4) }
     | '[' exps ']'          { EList (locate $1) (reverse $2)       }
     | '(' ')'               { ETuple (locate $1) []                }
     | '(' exp ')'           { $2                                   }

exps :: { [Expr Location] }
exps : {- empty -}          { []      }
     | exp                  { [$1]    }
     | exps ',' exp         { $3 : $1 }

type :: { Type }
type : btype '->' type    { TyArr $1 $3 }
     | btype              { $1 }

btype :: { Type }
bype  : btype atype { locatedError (locate $1) "Type application unsupported" }
      | atype       { $1 }

atype :: { Type }
atype : uid { TyLit ($1^?!tid) }
      | lid { TyVar ($1^?!tid) }
      | '[' type ']' { TyList $2 }

{
parse :: [Token Location] -> Expr Location
parse [] = EEmpty (Location 0 0)
parse ts = parseRaw ts

parseError :: [Token Location] -> a
parseError []     = errorWithoutStackTrace "Parse error at unknown location"
parseError (t:ts) = locatedError (locate t) "Parse error"
}
