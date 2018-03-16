{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Lang where

import Control.DeepSeq          (NFData)
import Data.Text                (Text)
import qualified Data.Text as T
import GHC.Generics             (Generic)

import Location

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

data Token =
    TLParen { tloc :: Location }
  | TEof    { tloc :: Location }
  | TRParen { tloc :: Location }
  | TLte    { tloc :: Location }
  | TComma  { tloc :: Location }
  | TDColon { tloc :: Location }
  | TColon  { tloc :: Location }
  | TLBrace { tloc :: Location }
  | TRBrace { tloc :: Location }
  | TEqual  { tloc :: Location }
  | TFun    { tloc :: Location }
  | TFix    { tloc :: Location }
  | TRArrow { tloc :: Location }
  | TIf     { tloc :: Location }
  | TThen   { tloc :: Location }
  | TElse   { tloc :: Location }
  | TLet    { tloc :: Location }
  | TIn     { tloc :: Location }
  | TPlus   { tloc :: Location }
  | TMinus  { tloc :: Location }
  | TTimes  { tloc :: Location }
  | TDivide { tloc :: Location }
  | TBool   { tloc :: Location, tbool :: !Bool }
  | TLid    { tloc :: Location, tid :: !Name }
  | TUid    { tloc :: Location, tid :: !Name }
  | TInt    { tloc :: Location, tint ::{-# UNPACK #-} !Int }
  | TFloat  { tloc :: Location, tfloat :: {-# UNPACK #-} !Double }
  deriving (Generic)

instance NFData Token

instance Located Token where
  locate = tloc
  {-# INLINE locate #-}

data Type =
    TyLit Name
  | TyVar Name
  | TyGenSym {-# UNPACK #-} !Int
  | TyTuple [Type]
  | TyList Type
  | TyArr Type Type
  deriving (Generic,Eq)

instance NFData Type

instance Located Type where
  locate = const NoLocation
  {-# INLINE locate #-}

data Expr =
    EEmpty  { eloc :: Location }
  | ECons   { eloc :: Location, eelem :: Expr, elist :: Expr }
  | ESig    { eloc :: Location, eexp :: Expr, etype :: Type }
  | EInt    { eloc :: Location, eint :: {-# UNPACK #-} !Int }
  | EFloat  { eloc :: Location, efloat ::  {-# UNPACK #-} !Double }
  | EVar    { eloc :: Location, evar :: !Name }
  | EBool   { eloc :: Location, ebool :: !Bool }
  | ETuple  { eloc :: Location, eelems :: ![Expr] }
  | EList   { eloc :: Location, eelems :: ![Expr] }
  | EApp    { eloc :: Location, eapp1 :: !Expr, eapp2 :: !Expr }
  | EOp     { eloc :: Location, eop :: !Op, eopp1 :: !Expr, eopp2 :: !Expr }
  | EIf     { eloc :: Location, eif :: !Expr, ethen :: Expr, eelse :: Expr }
  | ELet    { eloc :: Location, ename :: !Name, ebind :: Expr, ein :: Expr }
  | EFix    { eloc :: Location, efun :: !Name, evar :: !Name, ebody :: Expr }
  | ELam    { eloc :: Location, evar :: !Name, ebody :: Expr }
  deriving (Generic)

instance NFData Expr

instance Located Expr where
  locate  = eloc
  {-# INLINE locate #-}

opType :: Type -> Op -> Type
opType _ Lte = TyLit "Bool"
opType t _   = t

ppOp :: Op -> Text
ppOp Plus   = "+"
ppOp Minus  = "-"
ppOp Times  = "*"
ppOp Divide = "/"
ppOp Lte    = "<="

ppExpr :: Expr -> Text
ppExpr (EEmpty _)          = ""
ppExpr (ECons _ e1 e2)     = T.concat [ppExpr e1, ":", ppExpr e2]
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

ppToken :: Token -> Text
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
ppToken (TEof _)        = ""

ppTokenList :: [Token] -> Text
ppTokenList ts = T.concat ["[", T.intercalate ", " (map ppToken ts), "]"]

ppType :: Type -> Text
ppType (TyLit t) = t
ppType (TyVar t) = t
ppType (TyGenSym i) = T.concat ["t", T.pack (show i)]
ppType (TyTuple ts) = T.concat ["(", T.intercalate "," (map ppType ts), ")"]
ppType (TyList t) = T.concat ["[", ppType t, "]"]
ppType (TyArr t1@TyArr{} t2) = T.concat ["(", ppType t1, ") -> ", ppType t2]
ppType (TyArr t1 t2) = T.concat [ppType t1, " -> ", ppType t2]

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
