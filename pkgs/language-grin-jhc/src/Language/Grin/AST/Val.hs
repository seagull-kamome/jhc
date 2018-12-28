module Language.Grin.AST.Val (
  Val(..), n0, n1, n2, n3, p0, p1, p2, p3, isVar,
  valType, valFreeVars, valFreeTagVars,
  valIsConstant,
  properHole, isHole
  ) where

import qualified Data.Text as T
import qualified Data.EnumSet.EnumSet as ESet

import Text.PrettyPrint.ANSI.Leijen hiding((<$>))

import Language.Grin.AST.Tag
import Language.Grin.AST.Var
import Language.Grin.AST.Type
import Language.Grin.Internal.Classes

-- ---------------------------------------------------------------------------

data Val sym primtypes primval
  = ValNodeC !(Tag sym) ![Val sym primtypes primval]
  | ValConst !(Val sym primtypes primval)
  | ValLit !Rational !(Typ primtypes)
  | ValVar !Var !(Typ primtypes)
  | ValUnit
  | ValPrim !primval ![Val sym primtypes primval] !(Typ primtypes)
  | ValIndex !(Val sym primtypes primval) !(Val sym primtypes primval)
  | ValItem !sym !(Typ primtypes)
  | ValUnknown !(Typ primtypes)
  deriving (Eq, Ord)


instance (Pretty (Tag sym), Pretty sym, Pretty (Typ primtypes),
          PrimType primtypes, Pretty primval)
    => Pretty (Val sym primtypes primval) where
  pretty = \case
    ValNodeC t [] -> pretty t
    ValNodeC t vs -> pretty t <+> hsep (map pretty vs)
    ValConst v -> char '&' <> pretty v
    ValLit l _ -> text $ show l
    ValVar v@(Var n) t -> f t
      where f = \case
              TypPtr t' -> char 'p' <> f t'
              TypNode -> "ng" <> int n
              TypINode -> "ni" <> int n
              TypPrim prim -> text (T.unpack $ varPrefix prim) <> int n
              TypRegion -> "m" <> int n
              TypGcContext -> "gc" <> int n
              TypRegister t' -> "r" <> f t'
              _ -> pretty v
    ValUnit -> text "()"
    ValPrim x args ty -> pretty x <> tupled (map pretty args) <> text "::" <> pretty ty
    ValIndex p ofs -> pretty p <> brackets (pretty ofs)
    ValItem x ty -> pretty x <> text "::" <> pretty ty
    ValUnknown ty -> text "?::" <> pretty ty


-- ---------------------------------------------------------------------------


n0, n1, n2, n3, p0, p1, p2, p3 :: Val sym primtypes primval
n0 = ValVar v0 TypNode
n1 = ValVar v1 TypNode
n2 = ValVar v2 TypNode
n3 = ValVar v3 TypNode
p0 = ValVar v0 TypINode
p1 = ValVar v1 TypINode
p2 = ValVar v2 TypINode
p3 = ValVar v3 TypINode




isVar :: Val sym primtypes primval -> Bool
isVar (ValVar _ _) = True
isVar _ = False



-- | Resolve type of the value
valType :: Monad m => Val sym primtypes primval -> m (Typ primtypes)
valType (ValNodeC _ _) = pure TypNode
valType (ValConst x) = valType x >>= \case
  TypNode -> pure TypINode
  _ -> fail "Val.getType: Const of non-node."
valType (ValLit _ t) = pure t
valType (ValVar _ t) = pure t
valType ValUnit = pure TypUnit
valType (ValPrim _ _ t) = pure t
valType (ValIndex v _) = valType v
valType (ValItem _ t) = pure t
valType (ValUnknown t) = pure t



valFreeVars :: Enum (Tag sym) => [Val sym primtypes primval] -> ESet.EnumSet Var
valFreeVars = ESet.unions . map (\case
  ValNodeC _ xs -> valFreeVars xs
  ValConst v    -> valFreeVars [v]
  ValIndex x y  -> valFreeVars [x, y]
  ValVar v _    -> ESet.singleton v
  _ -> ESet.empty)



valFreeTagVars :: Enum (Tag sym) => [Val sym primtypes primval] -> ESet.EnumSet (Tag sym)
valFreeTagVars = ESet.unions . map (\case
  ValNodeC t xs -> ESet.insert t $ valFreeTagVars xs
  ValConst v -> valFreeTagVars [v]
  ValIndex x y -> valFreeTagVars [x, y]
  _ -> ESet.empty )



valIsConstant :: Val sym primtype primval -> Bool
valIsConstant = \case
  ValNodeC _ xs -> all valIsConstant xs
  ValLit{}   -> True
  ValConst{} -> True
  ValVar (Var v) _ -> v < 0
  ValIndex v t -> valIsConstant v && valIsConstant t
  ValPrim{} -> True
  _ -> False




-- ---------------------------------------------------------------------------
--

-- | ???
properHole :: Typ primtypes -> Val sym primtypes primval
properHole x = case x of
  TypINode -> ValConst $ ValNodeC (Tag $ TagHole 0) []
  TypPrim _ -> ValLit 0 x
  TypNode -> ValNodeC (Tag $ TagHole 0) []
  _ -> error "No proper hole"


isHole :: Eq (Val sym primtypes primval) => Val sym primtypes primval -> Bool
isHole x = x == y || x == ValConst y
  where y = ValNodeC (Tag $ TagHole 0) []



-- vim: ts=8 sw=2 expandtab :


