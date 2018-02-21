{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}
module Lang
  ( Token(..)
  , Expr(..)
  , Op(..)
  , Value(..)
  , Name
  , ppTokenList
  , ppExpr
  , ppValue
  ) where

import Control.DeepSeq          (NFData)
import Data.Text                (Text)
import qualified Data.Text as T
import GHC.Generics             (Generic)

data Value =
    VEmpty
  | VInt   {-# UNPACK #-} !Int
  | VFloat {-# UNPACK #-} !Double
  | VBool  !Bool
  deriving (Generic)

instance NFData Value

data Op =
    Plus
  | Minus
  | Times
  | Divide
  | Lte
  deriving (Generic)

instance NFData Op

data Token a =
    TLParen a
  | TRParen a
  | TLte    a
  | TEqual  a
  | TFun    a
  | TFix    a
  | TRArrow a
  | TIf     a
  | TThen   a
  | TElse   a
  | TLet    a
  | TIn     a
  | TPlus   a
  | TMinus  a
  | TTimes  a
  | TDivide a
  | TBool   a !Bool
  | TId     a !Name
  | TInt    a {-# UNPACK #-} !Int
  | TFloat  a {-# UNPACK #-} !Double
  deriving (Generic)

instance (NFData a) => NFData (Token a)

type Name = Text

-- n.b. No strictness in branches of EIf to save work.
data Expr a =
    EEmpty
  | EInt    a {-# UNPACK #-} !Int
  | EFloat  a {-# UNPACK #-} !Double
  | EVar    a !Name
  | EBool   a !Bool
  | EApp    a !(Expr a) !(Expr a)
  | EOp     a !Op !(Expr a) !(Expr a)
  | EIf     a !(Expr a) (Expr a) (Expr a)
  | ELet    a !Name     (Expr a) (Expr a)
  | EFix    a !Name    !Name     (Expr a)
  | ELam    a !Name     (Expr a)
  deriving (Generic)

instance (NFData a) => NFData (Expr a)

ppOp :: Op -> Text
ppOp Plus   = "+"
ppOp Minus  = "-"
ppOp Times  = "*"
ppOp Divide = "/"
ppOp Lte    = "<="

ppExpr :: Expr a -> Text
ppExpr EEmpty              = ""
ppExpr (EVar _ n)          = n
ppExpr (EInt _ n)          = T.pack $ show n
ppExpr (EFloat _ f)        = T.pack $ show f
ppExpr (EBool _ True)      = "true"
ppExpr (EBool _ False)     = "false"
ppExpr (EApp _ e1 e2)      = T.concat ["(", ppExpr e1, " ", ppExpr e2, ")"]
ppExpr (EOp _ o e1 e2)     = T.concat ["(", ppOp o, " ",  ppExpr e1, " ", ppExpr e2, ")"]
ppExpr (EIf _ e1 e2 e3)    = T.concat ["(if ", ppExpr e1, " ", ppExpr e2, " ", ppExpr e3, ")"]
ppExpr (ELet _ n e1 e2)    = T.concat ["(let (", n, " = ", ppExpr e1, ") ", ppExpr e2, ")"]
ppExpr (EFix _ f n e)      = T.concat ["(fix ", f, " ", n, " -> ", ppExpr e, ")"]
ppExpr (ELam _ n e)        = T.concat ["(fun ", n, " -> ", ppExpr e, ")"]

ppToken :: Token a -> Text
ppToken (TLParen _)     = "("
ppToken (TRParen _)     = ")"
ppToken (TLte _)        = "<="
ppToken (TEqual _)      = "="
ppToken (TRArrow _)     = "->"
ppToken (TIf _)         = "if"
ppToken (TThen _)       = "then"
ppToken (TElse _)       = "else"
ppToken (TLet _)        = "let"
ppToken (TIn _)         = "in"
ppToken (TFun _)        = "fun"
ppToken (TFix _)        = "fix"
ppToken (TPlus _)       = "+"
ppToken (TMinus _)      = "-"
ppToken (TTimes _)      = "*"
ppToken (TDivide _)     = "/"
ppToken (TBool _ True)  = "true"
ppToken (TBool _ False) = "false"
ppToken (TId _ t)       = t
ppToken (TInt _ n)      = T.pack $ show n
ppToken (TFloat _ f)    = T.pack $ show f

ppTokenList :: [Token a] -> Text
ppTokenList ts = T.concat ["[", T.intercalate ", " (map ppToken ts), "]"]

-- n.b. VBools are handled differently because the object language uses
-- lowercase for booleans, whereas Haskell uses uppercase identifiers.
ppValue :: Value -> Text
ppValue VEmpty     = ""
ppValue (VInt i)   = T.pack $ show i
ppValue (VFloat f) = T.pack $ show f
ppValue (VBool b)  = if b then "true" else "false"
{-# INLINE ppValue #-}
