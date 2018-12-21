module Language.Grin.AST.Expression (
  FuncDef(..), newFuncDef, updateFuncDef,
  --
  Expression''(..), Expression(..)
  ) where

import qualified Data.Text as T
import qualified Data.Set as Set

import Language.Grin.AST.Tag
import Language.Grin.AST.Var
import Language.Grin.AST.Type
import Language.Grin.AST.Lambda
import Language.Grin.AST.BasicOperation
import Language.Grin.Data.Perhaps
import Language.Grin.Data.FuncProp
import Language.Grin.Internal.Classes


-- ---------------------------------------------------------------------------

data FuncDef sym primtypes littyp primval expr = FuncDef {
    funcDefName :: !sym,
    funcDefBody :: !(Lambda sym primtypes littyp expr),
    funcDefCall :: !(Val sym primtypes littyp primval),
    funcDefProps :: !FuncProps sym (Typ primtypes)
  }
  deriving (Show, Eq, Ord)



newFuncDef :: Expr sym primtypes expr
           => Bool -> sym -> Lambda sym primtypes littyp expr
           -> FuncDef sym primtypes littyp primval expr
newFuncDef local name lam = FuncDef {
    funcDefName = name,
    funcDefBody = lam,
    funcDefCall = call,
    funcDefProps = newFuncProps lam }
  where
    props = newFunctopnProps lam
    call = ValItem name
                   (TyCall (if local then LocalFunction else Function)
                           (fst $ funcType prop)
                           (snd $ funcType prop))


updateFuncDefProps :: Expr sym primtypes expr
                   => Lambda sym primtypes littyp primval expr
                   -> FuncDef sym primtypes littyp primval expr
                   -> FuncDef sum primtypes littyp primval expr
updateFuncDefProps lam fd@FuncDef{..} =
  fd { funcDefProps = updateFuncProps funcDefProps }




-- ---------------------------------------------------------------------------


data Expression'' sym primetypes primopr littyp primval expr
  = ExprBind !expr !(Lambda (Val sum primtypes littyp primval) expr)
  | ExprBaseOp {
      expBaseOp :: !(BasicOperation primtypes),
      expArgs :: ![Val sym primtypes littyp primval] }
  | ExprApp {
      expFunction :: !sym,
      expArgs :: ![Val sym primtypes littyp primval],
      expType :: ![Typ primtypes] }
  | ExprPrim {
      expPrimitive :: primopr,
      expArgs :: ![Val sym primtypes littyp primval],
      expType :: ![Typ primtypes] }
  | ExpCase {
      expValue :: !(Val sym primtypes littyp primval),
      expAlts :: ![Lambda sym primtypes littyp expr] }
  | ExpReturn {
      expValues :: ![Val sym primtypes, littyp primval] }
  | ExpError {
      expError :: T.Text,
      expType :: ![Typ primtypes] }
  | ExpCall {
      expValue :: !(Val sym primtypes littyp primval),
      expArgs :: ![Val sym primtypes littyp primval],
      expType :: ![Typ primtypes],
      expJump :: !Bool,
      expFuncProps :: !FuncProps sym primtypes,
      expInfo :: !Info.Info {- is this a pragma or analysis result? -} }
  | ExpNewRegion { expLam :: Lambda val expr, expInfo :: Info.Info }
  | ExpAlloc {
      expValue :: !(Val sym primtypes littyp primval),
      expCount :: !(Val sym primtypes littyp primval),
      expRegion :: !(Val sym primtypes littyp primval),
      expInfo :: Info.Info}
  | ExpLet {
      expDefs :: ![FuncDef sym primtypes littyp primval expr],
      expBody :: !expr,
      expFunCalls :: !(Set.Set sym, Set.Set sym),
      expIsNormal :: !Bool,
      expNonNormal :: Set.Set sym,
      expInfo :: Info.Info }
  | MkClosure {
      expValue :: !(Val sym primtypes littyp primval),
      expArgs :: ![Val sym primtypes littyp primval],
      expRegion :: !(Val sym primtypes littyp primval),
      expType :: ![Type primpypes],
      expInfo :: Info.Info }
  | MkCont {
      expCont :: Lambda sym primtypes littyp expr,
      expLam :: Lambda sym prumtypes littyp expr,
      expInfo :: Info.Info }
  | GcRoots {
      expValue :: ![Val sym primtypes littyp prinval],
      expBody :: !expr }
  deriving (Show, Eq, Ord)

newtype Expression sym primtypes primopr littyp primval
  = Expression {
    unwrap :: Expression'' sym primtypes primopr littyp primval
                           (Expression sym primtypes primopr littyp primval)
  }
  deriving (Show, Eq, Ord)


// TODO:
instance Expr sym primtypes (Expression sym primtypes primopr littyp primval) where
  exprFreeVars =
  exprType =



-- vim: ts=8 sw=2 expandtab :

