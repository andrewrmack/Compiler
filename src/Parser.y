{
module Parser (parse) where

import Data.Text (Text)
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
      '='     { TEqual  _   }
      '\\'    { TBSlash _   }
      '->'    { TRArrow _   }
      if      { TIf     _   }
      then    { TThen   _   }
      else    { TElse   _   }
      let     { TLet    _   }
      in      { TIn     _   }
      '+'     { TPlus   _   }
      '-'     { TMinus  _   }
      '*'     { TTimes  _   }
      '/'     { TDivide _   }
      bool    { TBool   _ _ }
      id      { TId     _ _ }
      int     { TInt    _ _ }
      float   { TFloat  _ _ }

%%

exp  :: { Expr Located }
exp  : if exp then exp else exp { EIf (locate $1) $2 $4 $6          }
     | let id '=' exp in exp    { ELet (locate $1) (getId $2) $4 $6 }
     | '\\' id '->' exp         { ELam (locate $1) (getId $2) $4    }
     | fexp                     { $1                                }

fexp :: { Expr Located }
fexp : fexp exp1                { EApp (locateExp $1) $1 $2         }
     | exp1                     { $1                                }

exp1 :: { Expr Located }
exp1 : exp1 '+'  exp1           { EOp (locate $2) Plus $1 $3        }
     | exp1 '-'  exp1           { EOp (locate $2) Minus $1 $3       }
     | exp1 '*'  exp1           { EOp (locate $2) Times $1 $3       }
     | exp1 '/'  exp1           { EOp (locate $2) Divide $1 $3      }
     | exp1 '<=' exp1           { EOp (locate $2) Lte $1 $3         }
     | int                      { EInt (locate $1) (getInt $1)      }
     | float                    { EFloat (locate $1) (getFloat $1)  }
     | bool                     { EBool (locate $1) (getBool $1)    }
     | id                       { EVar (locate $1) (getId $1)       }
     | '(' exp ')'              { $2                                }


{
-- I hate that this is necessary, but there doesn't seem to be a great
-- solution to this in Haskell.
locate :: Token Located -> Located
locate (TLParen l)  = l
locate (TRParen l)  = l
locate (TLte l)     = l
locate (TEqual l)   = l
locate (TIf l)      = l
locate (TThen l)    = l
locate (TElse l)    = l
locate (TLet l)     = l
locate (TIn l)      = l
locate (TPlus l)    = l
locate (TMinus l)   = l
locate (TTimes l)   = l
locate (TDivide l)  = l
locate (TBool l _)  = l
locate (TId l _)    = l
locate (TInt l _)   = l
locate (TFloat l _) = l

locateExp :: Expr Located -> Located
locateExp (EInt   l _)     = l
locateExp (EFloat l _)     = l
locateExp (EVar   l _)     = l
locateExp (EBool  l _)     = l
locateExp (EApp   l _ _)   = l
locateExp (EOp    l _ _ _) = l
locateExp (EIf    l _ _ _) = l
locateExp (ELet   l _ _ _) = l
locateExp (ELam   l _ _)   = l

parse :: [Token Located] -> Expr Located
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

parseError :: [Token Located] -> a
parseError []     = errorWithoutStackTrace "Parse error at unknown location"
parseError (t:ts) = locatedError (locate t) "Parse error"
}
