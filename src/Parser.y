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
%tokentype { Token }
%error { parseError }

%nonassoc '<='
%left '+' '-'
%left '*' '/'
%right ':'

%token
      '('     { TLParen _   }
      ')'     { TRParen _   }
      '<='    { TLte    _   }
      ','     { TComma  _   }
      '::'    { TDColon _   }
      ':='    { TCEq    _   }
      '!'     { TBang   _   }
      ';'     { TSemi   _   }
      ':'     { TColon  _   }
      '['     { TLBrace _   }
      ']'     { TRBrace _   }
      '<'     { TLAngle _   }
      '>'     { TRAngle _   }
      '='     { TEqual  _   }
      ref     { TRef    _   }
      fun     { TFun    _   }
      fix     { TFix    _   }
      '->'    { TRArrow _   }
      while   { TWhile  _   }
      do      { TDo     _   }
      end     { TEnd    _   }
      Array   { TArray  _   }
      if      { TIf     _   }
      then    { TThen   _   }
      else    { TElse   _   }
      let     { TLet    _   }
      new     { TNew    _   }
      in      { TIn     _   }
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

-- Weirdly, building our linked list backwards doesn't matter
-- for our evaluator or typechecker, and the parser likes it
texp :: { Expr }
texp : texp ';' exp        { ESeq (locate $1) $1 $3       }
     | exp                 { $1 }

exp  :: { Expr }
exp  : iexp '::' type     { ESig (locate $1) $1 $3       }
     | iexp               { $1                           }

iexp :: { Expr }
iexp : iexp '+'  iexp     { EOp (locate $2) Plus $1 $3   }
     | iexp '-'  iexp     { EOp (locate $2) Minus $1 $3  }
     | iexp '*'  iexp     { EOp (locate $2) Times $1 $3  }
     | iexp '/'  iexp     { EOp (locate $2) Divide $1 $3 }
     | iexp '<=' iexp     { EOp (locate $2) Lte $1 $3    }
     | iexp ':'  iexp     { ECons (locate $2) $1 $3      }
     | iexp ':=' iexp     { EAssign (locate $2) $1 $3    }
     | lexp               { $1                           }

lexp :: { Expr }
lexp : if exp then texp else texp  { EIf (locate $1) $2 $4 $6                  }
     | while exp do texp end       { EWhile (locate $1) $2 $4                  }
     | let lid '=' exp in texp     { ELet (locate $1) ($2^?!tid) $4 $6         }
     | fun lid '->' texp           { ELam (locate $1) ($2^?!tid) $4            }
     | fix lid lid '->' texp       { EFix (locate $1) ($2^?!tid) ($3^?!tid) $5 }
     | fexp                        { $1                                        }

fexp :: { Expr }
fexp : fexp aexp          { EApp (locate $1) $1 $2 }
     | aexp               { $1                     }

aexp :: { Expr }
aexp : int                   { EInt (locate $1) ($1^?!tint)         }
     | float                 { EFloat (locate $1) ($1^?!tfloat)     }
     | bool                  { EBool (locate $1) ($1^?!tbool)       }
     | lid                   { EVar (locate $1) ($1^?!tid)          }
     | ref aexp              { ERef (locate $1) $2                  }
     | '!' aexp              { EDeref (locate $1) $2                }
     | new type '<' int '>'  { ENewArr (locate $1) $2 ($4^?!tint)   }
     | aexp '<' exp '>'      { EArrAcc (locate $1) $1 $3            }
     | '(' exp ',' exps ')'  { ETuple (locate $1) ($2 : reverse $4) }
     | '[' exps ']'          { EList (locate $1) (reverse $2)       }
     | '(' ')'               { ETuple (locate $1) []                }
     | '(' exp ')'           { $2                                   }

exps :: { [Expr] }
exps : {- empty -}          { []      }
     | exp                  { [$1]    }
     | exps ',' exp         { $3 : $1 }

type :: { Type }
type : btype '->' type    { TyArr $1 $3 }
     | btype              { $1 }

btype :: { Type }
bype  : Array atype { TyArray $2 }
      | atype       { $1 }

atype :: { Type }
atype : uid { TyLit ($1^?!tid) }
      | lid { TyVar ($1^?!tid) }
      | '[' type ']' { TyList $2 }
      | '<' type '>' { TyRef  $2 }

{
parse :: [Token] -> Expr
parse [] = EEmpty (Location 0 0)
parse ts = parseRaw ts

parseError :: [Token] -> a
parseError []     = errorWithoutStackTrace "Parse error at unknown location"
parseError (t:ts) = locatedError (locate t) "Parse error"
}
