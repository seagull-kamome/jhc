module Language.Grin.AST.Expression (
  Var(..), v0, v1, v2, v3,
  --
  Val''(..), Val, n0, n1, n2, n3, p0, p1, p2, p3, isVar,
  --
  Lambda''(..),
  --
  Expression''(..), Expression(..)
  ) where

import qualified Data.Text as T

import Language.Grin.AST.Tag
import Language.Grin.AST.Type
import Language.Grin.AST.BasicOperation

-- ---------------------------------------------------------------------------

newtype Var = Var Int deriving (Eq, Ord, Enum)

v0, v1, v2, v3 :: Var
v0 = Var 0
v1 = Var 1
v2 = Var 2
v3 = Var 3



-- ---------------------------------------------------------------------------

data Val'' sym typ primval
  = ValNodeC !(Tag sym) ![Val'' sym typ primval]
  | ValConst !(Val'' sym typ primval)
  | ValLit !Number !(typ
  | ValVar !Var !typ
  | ValUnit
  | ValPrim !primval ![Val'' sym typ primval] !typ
  | ValIndex !(Val'' sym typ primval) !(Val'' sym typ primval)
  | ValItem !sym !typ
  | ValUnknown !typ
  deriving (Eq, Ord)

type Val sym primtypes primval = Val'' sym (Typ primtypes) primval


n0, n1, n2, n3, p0, p1, n2, p3 :: Val'' sym primtypes primval
n0 = ValVar v0 $ Typ TypNode
n1 = ValVar v1 $ Typ TypNode
n2 = ValVar v2 $ Typ TypNode
n3 = ValVar v3 $ Typ TypNode
p0 = ValVar v0 $ Typ TypINode
p1 = ValVar v1 $ Typ TypINode
p2 = ValVar v2 $ Typ TypINode
p3 = ValVar v3 $ Typ TypINode


isVar :: Val'' _ _ _
isVar (ValVar _ _) = True
isVar _ = False





-- ---------------------------------------------------------------------------

data Lambda'' val expr = Lambda { lamBind :: ![val], lamExp :: ! expr }


data Expression'' sym opr primopr val typ expr
  = ExprBind !expr !(Lambda'' val expr)
  | ExprBaseOp { expBaseOp :: !opr, expArgs :: ![val] }
  | ExprApp { expFunction :: !sym,  expArgs :: ![val], expType :: ![typ] }
  | ExprPrim { expPrimitive :: primopr, expArgs :: ![val], expType :: ![typ] }
  | ExpCase { expValue :: !val, expAlts :: ![Lambda'' val expr] }
  | ExpReturn { expValues :: ![val] }
  | ExpError { expError :: T.Text, expType :: ![typ] }
  | ExpCall {
      expValue :: !val,
      expArgs :: ![val],
      expType :: ![typ],
      expJump :: !Bool,
      expFUncProps :: !FuncProps,
      expInfo :: !Info.Info {- is this a pragma or analysis result? -} }
  | ExpNewRegion { expLam :: Lambda'' val expr, expInfo :: Info.Info }
  | ExpAlloc {
      expValue :: !val,
      expCount :: !val,
      expRegion :: !val,
      expInfo :: Info.Info}
  | ExpLet {
      expDefs :: ![FuncDef],
      expBody :: !expr,
      expFunCalls :: !(Set.Set sym, Set.Set sym),
      expIsNormal :: !Bool,
      expNonNormal :: Set.Set sym,
      expInfo :: Info.Info }
  | MkClosure {
      expValue :: !val,
      expArgs :: ![val],
      expRegion :: !val,
      expType :: ![typ],
      expInfo :: Info.Info }
  | MkCont {
      expCont :: Lambda'' val expr,
      expLam :: Lambda'' val expr,
      expInfo :: Info.Info }
  | GcRoots { expValue :: ![val], expBody :: !expr }
  deriving (Show, Eq, Ord)

newtype Expression sym primopr primtypes primval = Expression {
    unwrap :: Expression'' sym
                (BasicOperation primtypes)
                primopr
                (Val sym primtypes primval)
                (Typ primtypes)
                (Expression sym primopr primtypes primval)
  }


