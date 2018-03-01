{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Lang
  ( Token(..)
  , Expr(..)
  , Op(..)
  , Value(..)
  , Type(..)
  , Name
  , ttag
  , tbool
  , tint
  , tfloat
  , tid
  , etag
  , ppTokenList
  , ppExpr
  , ppValue
  ) where

import Control.DeepSeq          (NFData)
import Control.Lens             (makeLenses, makeLensesFor)
import Data.Text                (Text)
import qualified Data.Text as T
import GHC.Generics             (Generic)

data Value =
    VEmpty
  | VInt   {-# UNPACK #-} !Int
  | VFloat {-# UNPACK #-} !Double
  | VBool  !Bool
  | VTuple [Value]
  | VList [Value]
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

type Name = Text

data Token a =
    TLParen { _ttag :: a }
  | TRParen { _ttag :: a }
  | TLte    { _ttag :: a }
  | TComma  { _ttag :: a }
  | TDColon { _ttag :: a }
  | TColon  { _ttag :: a }
  | TLBrace { _ttag :: a }
  | TRBrace { _ttag :: a }
  | TEqual  { _ttag :: a }
  | TFun    { _ttag :: a }
  | TFix    { _ttag :: a }
  | TRArrow { _ttag :: a }
  | TIf     { _ttag :: a }
  | TThen   { _ttag :: a }
  | TElse   { _ttag :: a }
  | TLet    { _ttag :: a }
  | TIn     { _ttag :: a }
  | TPlus   { _ttag :: a }
  | TMinus  { _ttag :: a }
  | TTimes  { _ttag :: a }
  | TDivide { _ttag :: a }
  | TBool   { _ttag :: a, _tbool :: !Bool }
  | TLid    { _ttag :: a, _tid :: !Name }
  | TUid    { _ttag :: a, _tid :: !Name }
  | TInt    { _ttag :: a, _tint ::{-# UNPACK #-} !Int }
  | TFloat  { _ttag :: a, _tfloat :: {-# UNPACK #-} !Double }
  deriving (Generic)

makeLenses ''Token

instance (NFData a) => NFData (Token a)

data Type =
    TyLit Name
  | TyVar Name
  | TyTuple [Type]
  | TyList Type
  | TyArr Type Type
  deriving (Generic)

instance NFData Type

-- n.b. No strictness in branches of EIf to save work.
data Expr a =
    EEmpty  { _etag :: a }
  | ESig    { _etag :: a, _eexp :: Expr a, _etype :: Type }
  | EInt    { _etag :: a, _eint :: {-# UNPACK #-} !Int }
  | EFloat  { _etag :: a, _efloat ::  {-# UNPACK #-} !Double }
  | EVar    { _etag :: a, _evar :: !Name }
  | EBool   { _etag :: a, _ebool :: !Bool }
  | ETuple  { _etag :: a, _eelems :: ![Expr a] }
  | EList   { _etag :: a, _eelems :: ![Expr a] }
  | EApp    { _etag :: a, _eapp1 :: !(Expr a), _eapp2 :: !(Expr a) }
  | EOp     { _etag :: a, _eop :: !Op, _eopp1 :: !(Expr a), _eopp2 :: !(Expr a) }
  | EIf     { _etag :: a, _eif :: !(Expr a), _ethen :: Expr a, _eelse :: Expr a }
  | ELet    { _etag :: a, _ename :: !Name, _ebind :: Expr a, _ein :: Expr a }
  | EFix    { _etag :: a, _efun :: !Name, _evar :: !Name, _ebody :: Expr a }
  | ELam    { _etag :: a, _evar :: !Name, _ebody :: Expr a }
  deriving (Generic)

makeLensesFor [("_etag", "etag")] ''Expr

instance (NFData a) => NFData (Expr a)

ppOp :: Op -> Text
ppOp Plus   = "+"
ppOp Minus  = "-"
ppOp Times  = "*"
ppOp Divide = "/"
ppOp Lte    = "<="

ppExpr :: Expr a -> Text
ppExpr (EEmpty _)          = ""
ppExpr (ESig _ e _)        = ppExpr e
ppExpr (EVar _ n)          = n
ppExpr (EInt _ n)          = T.pack $ show n
ppExpr (EFloat _ f)        = T.pack $ show f
ppExpr (EBool _ True)      = "true"
ppExpr (EBool _ False)     = "false"
ppExpr (ETuple _ es)       = T.concat ["(", T.intercalate "," (map ppExpr es), ")"]
ppExpr (EList  _ es)       = T.concat ["[", T.intercalate "," (map ppExpr es), "]"]
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
ppToken (TComma _)      = ","
ppToken (TDColon _)     = "::"
ppToken (TColon _)      = ":"
ppToken (TLBrace _)     = "["
ppToken (TRBrace _)     = "]"
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
ppToken (TLid _ t)      = t
ppToken (TUid _ t)      = t
ppToken (TInt _ n)      = T.pack $ show n
ppToken (TFloat _ f)    = T.pack $ show f

ppTokenList :: [Token a] -> Text
ppTokenList ts = T.concat ["[", T.intercalate ", " (map ppToken ts), "]"]

-- n.b. VBools are handled differently because the object language uses
-- lowercase for booleans, whereas Haskell uses uppercase identifiers.
ppValue :: Value -> Text
ppValue VEmpty      = ""
ppValue (VInt i)    = T.pack $ show i
ppValue (VFloat f)  = T.pack $ show f
ppValue (VBool b)   = if b then "true" else "false"
ppValue (VTuple es) = T.concat ["(", T.intercalate "," (map ppValue es), ")"]
ppValue (VList es)  = T.concat ["[", T.intercalate "," (map ppValue es), "]"]
{-# INLINE ppValue #-}
