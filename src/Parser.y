{
{-# LANGUAGE OverloadedStrings #-}
module Parser (parse) where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)

import Language.Expression
import Language.Token
import Language.Type
import Lexer
import Utility.Basic
import Utility.Error
import Utility.Location
}

%name parseRaw
%lexer {lexerP} { TEof NoLocation }
%monad {Alex}
%tokentype { Token }
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
      '\\'    { TBSlash _   }
      '::'    { TDColon _   }
      ':'     { TColon  _   }
      ';'     { TSemi   _   }
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

mexp :: { Expr }
mexp : {- empty -}        { EEmpty NoLocation            }
     | exp                { $1                           }

{- An exp with an optional trailing semicolon -}
expc :: { Expr }
expc : exp ';'            { $1                           }
     | exp                { $1                           }

exp  :: { Expr }
exp  : iexp '::' type     { ESig (locate $1) $1 $3       }
     | iexp               { $1                           }

iexp :: { Expr }
iexp : iexp '+'  iexp     { EOp (locate $2) "+"  $1 $3 }
     | iexp '-'  iexp     { EOp (locate $2) "-"  $1 $3 }
     | iexp '*'  iexp     { EOp (locate $2) "*"  $1 $3 }
     | iexp '/'  iexp     { EOp (locate $2) "/"  $1 $3 }
     | iexp '<=' iexp     { EOp (locate $2) "<=" $1 $3 }
     | iexp ':'  iexp     { ECons (locate $2) $1 $3    }
     | lexp               { $1                         }

lexp :: { Expr }
lexp : if expc then expc else expc { EIf (locate $1) $2 $4 $6                  }
     | let lid '=' exp in exp      { ELet (locate $1) (tid $2) $4 $6         }
     | '\\' lid '->' exp           { ELam (locate $1) (tid $2) $4            }
     | fix lid lid '->' exp        { EFix (locate $1) (tid $2) (tid $3) $5 }
     | fexp                        { $1                                        }

fexp :: { Expr }
fexp : fexp aexp          { EApp (locate $1) $1 $2 }
     | aexp               { $1                     }

aexp :: { Expr }
aexp : int                   { EInt (locate $1) (tint $1)         }
     | float                 { EFloat (locate $1) (tfloat $1)     }
     | bool                  { EBool (locate $1) (tbool $1)       }
     | lid                   { EVar (locate $1) (tid $1)          }
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
bype  : btype atype { locatedError (locate $1) "Type application unsupported" }
      | atype       { $1 }

atype :: { Type }
atype : uid { TyLit (tid $1) }
      | lid { TyVar (tid $1) }
      | '[' type ']' { TyList $2 }

{
parse :: ByteString -> Expr
parse bs = case runAlex bs parseRaw of
             Left s -> errorWithoutStackTrace s
             Right e -> e

parseError :: Token -> Alex a
parseError t = locatedError (locate t) "Parse error"
}
