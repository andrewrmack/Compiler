{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Lang where

import Data.Monoid
import Control.DeepSeq          (NFData)
import Control.Lens
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
    TLParen { _tloc :: Location }
  | TRParen { _tloc :: Location }
  | TLte    { _tloc :: Location }
  | TComma  { _tloc :: Location }
  | TDColon { _tloc :: Location }
  | TCEq    { _tloc :: Location }
  | TBang   { _tloc :: Location }
  | TSemi   { _tloc :: Location }
  | TColon  { _tloc :: Location }
  | TLBrace { _tloc :: Location }
  | TRBrace { _tloc :: Location }
  | TLAngle { _tloc :: Location }
  | TRAngle { _tloc :: Location }
  | TEqual  { _tloc :: Location }
  | TRef    { _tloc :: Location }
  | TFun    { _tloc :: Location }
  | TFix    { _tloc :: Location }
  | TRArrow { _tloc :: Location }
  | TWhile  { _tloc :: Location }
  | TDo     { _tloc :: Location }
  | TEnd    { _tloc :: Location }
  | TArray  { _tloc :: Location }
  | TIf     { _tloc :: Location }
  | TThen   { _tloc :: Location }
  | TElse   { _tloc :: Location }
  | TLet    { _tloc :: Location }
  | TIn     { _tloc :: Location }
  | TPlus   { _tloc :: Location }
  | TMinus  { _tloc :: Location }
  | TTimes  { _tloc :: Location }
  | TDivide { _tloc :: Location }
  | TBool   { _tloc :: Location, _tbool :: !Bool }
  | TLid    { _tloc :: Location, _tid :: !Name }
  | TUid    { _tloc :: Location, _tid :: !Name }
  | TInt    { _tloc :: Location, _tint ::{-# UNPACK #-} !Int }
  | TFloat  { _tloc :: Location, _tfloat :: {-# UNPACK #-} !Double }
  deriving (Generic)

makeLenses ''Token

instance NFData Token

instance Located Token where
  locate t = t^.tloc

data Type =
    TyLit Name
  | TyVar Name
  | TyGenSym {-# UNPACK #-} !Int
  | TyTuple [Type]
  | TyList Type
  | TyArr Type Type
  deriving (Generic,Eq)

makePrisms ''Type

instance NFData Type

instance Located Type where
  locate _ = NoLocation

data Expr =
    EEmpty  { _eloc :: Location }
  | ECons   { _eloc :: Location, _eelem :: Expr, _elist :: Expr }
  | ESig    { _eloc :: Location, _eexp :: Expr, _etype :: Type }
  | EInt    { _eloc :: Location, _eint :: {-# UNPACK #-} !Int }
  | EFloat  { _eloc :: Location, _efloat ::  {-# UNPACK #-} !Double }
  | EVar    { _eloc :: Location, _evar :: !Name }
  | EBool   { _eloc :: Location, _ebool :: !Bool }
  | ETuple  { _eloc :: Location, _eelems :: ![Expr] }
  | EList   { _eloc :: Location, _eelems :: ![Expr] }
  | EApp    { _eloc :: Location, _eapp1 :: !Expr, _eapp2 :: !Expr }
  | EOp     { _eloc :: Location, _eop :: !Op, _eopp1 :: !Expr, _eopp2 :: !Expr }
  | EIf     { _eloc :: Location, _eif :: !Expr, _ethen :: Expr, _eelse :: Expr }
  | ELet    { _eloc :: Location, _ename :: !Name, _ebind :: Expr, _ein :: Expr }
  | EFix    { _eloc :: Location, _efun :: !Name, _evar :: !Name, _ebody :: Expr }
  | ELam    { _eloc :: Location, _evar :: !Name, _ebody :: Expr }
  deriving (Generic)

makeLenses ''Expr

instance NFData Expr

instance Located Expr where
  locate e = e^.eloc

infixr 6 <+>
(<+>) :: Text -> Text -> Text
t1 <+> t2 = t1 <> " " <> t2
{-# INLINE (<+>) #-}

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
ppExpr (EEmpty _)       = ""
ppExpr (ECons _ e1 e2)  = ppExpr e1 <> ":" <> ppExpr e2
ppExpr (ESig _ e _)     = ppExpr e
ppExpr (EVar _ n)       = n
ppExpr (EInt _ n)       = T.pack $ show n
ppExpr (EFloat _ f)     = T.pack $ show f
ppExpr (EBool _ True)   = "true"
ppExpr (EBool _ False)  = "false"
ppExpr (ETuple _ es)    = "(" <> T.intercalate "," (map ppExpr es) <> ")"
ppExpr (EList  _ es)    = "[" <> T.intercalate "," (map ppExpr es) <> "]"
ppExpr (EApp _ e1 e2)   = "(" <> ppExpr e1 <+> ppExpr e2 <> ")"
ppExpr (EOp _ o e1 e2)  = "(" <> ppOp o <+> ppExpr e1 <+> ppExpr e2 <> ")"
ppExpr (EIf _ e1 e2 e3) = "(if " <> ppExpr e1 <+> ppExpr e2 <+> ppExpr e3 <> ")"
ppExpr (ELet _ n e1 e2) = "(let (" <> n <> " = " <> ppExpr e1 <> ") " <> ppExpr e2 <> ")"
ppExpr (EFix _ f n e)   = "(fix " <> f <+> n <+> "->" <+> ppExpr e <> ")"
ppExpr (ELam _ n e)     = "(fun " <> n <+> "->" <+> ppExpr e <> ")"

ppToken :: Token -> Text
ppToken (TLParen _)     = "("
ppToken (TRParen _)     = ")"
ppToken (TLAngle _)     = "<"
ppToken (TRAngle _)     = ">"
ppToken (TLte _)        = "<="
ppToken (TComma _)      = ","
ppToken (TCEq _)        = ":="
ppToken (TBang _)       = "!"
ppToken (TSemi _)       = ";"
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
ppToken (TRef _)        = "ref"
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

ppTokenList :: [Token] -> Text
ppTokenList ts = "[" <> T.intercalate ", " (map ppToken ts) <> "]"

ppType :: Type -> Text
ppType (TyLit t) = t
ppType (TyVar t) = t
ppType (TyGenSym i) = "t" <> T.pack (show i)
ppType (TyTuple ts) = T.concat ["(", T.intercalate "," (map ppType ts), ")"]
ppType (TyList t) = T.concat ["[", ppType t, "]"]
ppType (TyArr t1@TyArr{} t2) = T.concat ["(", ppType t1, ") -> ", ppType t2]
ppType (TyArr t1 t2)         = ppType t1 <+> "->" <+> ppType t2

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
