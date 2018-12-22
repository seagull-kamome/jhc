module Language.Grin.AST.Val (
  Val(..), n0, n1, n2, n3, p0, p1, p2, p3, isVar,
  valType, valFreeVars, valFreeTagVars
  ) where

import qualified Data.Text as T
import qualified Data.EnumSet.EnumSet as ESet

import Text.PrettyPrint.ANSI.Leijen hiding((<$>))

import Language.Grin.AST.Tag
import Language.Grin.AST.Var
import Language.Grin.AST.Type
import Language.Grin.Internal.Classes

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


instance (Pretty (Tag sym), Pretty littyp, PrimType primtypes)
    => Pretty (Val sym primtypes littyp primval) where
  pretty = \case
    ValNodeC t [] -> pretty t
    ValNodeC t vs -> pretty t <+> hsep (map pretty vs)
    ValConst v -> char '&' <> pretty v
    ValLit l _ -> pretty l
    ValVar v@(Var n) t -> case t of
      TypPtr t' -> char 'p' <> pretty (ValVar v t')
      TypNode -> "ng" <> int n
      TypINode -> "ni" <> int n
      TypPrim prim -> text (T.unpack $ varPrefix prim) <> int i
      TypRegion -> "m" <> int n
      TypGCContext -> "gc" <> int n
      TypRegister t' -> "r" <> pretty (VarVal v t')
      _ -> pretty v



-- ---------------------------------------------------------------------------


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



-- | Resolve type of the value
valType :: Monad m => Val sym primtypes littyp primval -> m (Typ primtypes)
valType (ValNodeC _ _) = pure TypNode
valType (ValConst x) = case getType x of
  TypNode -> pure TypINode
  _ -> fail "Val.getType: Const of non-node."
valType (ValLit _ t) = pure t
valType (ValVar _ t) = pure t
valType ValUnit = pure TypUnit
valType (ValPrim _ _ t) = pure t
valType (ValIndex v _) = valType v
valType (ValItem _ t) = pure t
valType (ValUnknown t) = pure t



valFreeVars ::Val _ _ _ _ -> ESet.EnumSet Var
valFreeVars = \case
  ValNodeC _ xs -> mconcat $ map valFreeVars xs
  ValConst v -> valFeeVars v
  ValIndex x y -> valFreeVars x <> valFreeVars y
  ValVar v _ -> ESet.singleton v
  _ -> ESet.empty



valFreeTagVars ::Val sym _ _ _ -> ESet.EnumSet (Tag sym)
valFreeTagVars = \case
  ValNodeC t xs -> ESet.insert t mconcat (map valFreeTagVars xs)
  ValConst v -> valFreeTagVars v
  ValIndex x y -> valFreeTagVars x <> valFreeTagVars y
  _ -> ESet.empty



valFreeTagVars' :: [Val sym _ _ _] -> ESet.EnumSet (Tag sym)
valFreeTagVars' = mconcat valFreeTagVars




