{
module Parser where

import Lang
}

%name parse
%tokentype { Token }
%error { parseError }

%token
      '('     { TLParen   }
      ')'     { TRParen   }
      '<='    { TLte      }
      if      { TIf       }
      op      { TOp    $$ }
      bool    { TBool  $$ }
      int     { TInt   $$ }
      float   { TFloat $$ }

%%

exp :: { Expr }
exp : '(' op exp exp ')'     { EOp $2 $3 $4 }
    | '(' '<=' exp exp ')'   { ELte $3 $4   }
    | '(' if exp exp exp ')' { EIf $3 $4 $5 }
    | int                    { EInt $1      }
    | float                  { EFloat $1    }
    | bool                   { EBool $1     }


{
parseError :: [Token] -> a
parseError _ = locatedError r c "Parse error"
  where
    r = 0
    c = 0


locatedError :: Int -> Int -> String -> a
locatedError r c msg = errorWithoutStackTrace locatedMsg
  where
    locatedMsg = msg ++ " at (" ++ show r ++ ":" ++ show c ++ ")"
}
