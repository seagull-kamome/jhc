module Language.Grin.AST.Val (
  Val(..), n0, n1, n2, n3, p0, p1, p2, p3, isVar
  ) where

import qualified Data.Text as T

import Language.Grin.AST.Tag
import Language.Grin.AST.Var

-- ---------------------------------------------------------------------------

data Val sym primtypes littyp primval
  = ValNodeC !(Tag sym) ![Val sym primtypes primval]
  | ValConst !(Val sym primtypes littyp primval)
  | ValLit !littype !(Typ primtypes)
  | ValVar !Var !(Typ primtypes)
  | ValUnit
  | ValPrim !primval ![Val sym primtypes littyp primval] !typ
  | ValIndex !(Val sym primtypes littyp primval) !(Val sym primtypes littyp primval)
  | ValItem !sym !(Typ primtypes)
  | ValUnknown !(Typ primtypes)
  deriving (Eq, Ord)



n0, n1, n2, n3, p0, p1, n2, p3 :: Val _ _ _ _
n0 = ValVar v0 $ Typ TypNode
n1 = ValVar v1 $ Typ TypNode
n2 = ValVar v2 $ Typ TypNode
n3 = ValVar v3 $ Typ TypNode
p0 = ValVar v0 $ Typ TypINode
p1 = ValVar v1 $ Typ TypINode
p2 = ValVar v2 $ Typ TypINode
p3 = ValVar v3 $ Typ TypINode


isVar :: Val _ _ _ _
isVar (ValVar _ _) = True
isVar _ = False


valType :: Val sym primtypes littyp primval -> Either T.Text (Typ primtypes)
valType (ValNodeC _ _) = Right TypNode
valType (ValConst x) = case getType x of
  TypNode -> Right TypINode
  _ -> Left "Val.getType: Const of non-node."
valType (ValLit _ t) = Right t
valType (ValVar _ t) = Right t
valType ValUnit = Right TypUnit
valType (ValPrim _ _ t) = Right t
valType (ValIndex v _) = valType v
valType (ValItem _ t) = Right t
valType (ValUnknown t) = Right t






