module Language.Grin.AST.Expression (
  FuncDef(..), newFuncDef, updateFuncDef,
  --
  Expression''(..), Expression(..)
  ) where

import qualified Data.Text as T
import qualified Data.Set as Set

import qualified Data.EnumSet.EnumSet as ESet -- containers-missing

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
                   (uncurry (TyCall (if local then LocalFunction else Function))
                            funcType prop)


updateFuncDefProps :: Expr sym primtypes expr
                   => Lambda sym primtypes littyp primval expr
                   -> FuncDef sym primtypes littyp primval expr
                   -> FuncDef sum primtypes littyp primval expr
updateFuncDefProps lam fd@FuncDef{..} =
  fd { funcDefProps = updateFuncProps funcDefProps }




-- ---------------------------------------------------------------------------


data Expression'' sym primetypes primopr littyp primval expr
  = ExprBind !expr !(Lambda (Val sym primtypes littyp primval) expr)
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
  | ExprCase {
      expValue :: !(Val sym primtypes littyp primval),
      expAlts :: ![Lambda sym primtypes littyp expr] }
  | ExprReturn {
      expValues :: ![Val sym primtypes, littyp primval] }
  | ExprError {
      expError :: T.Text,
      expType :: ![Typ primtypes] }
  | ExprCall {
      expValue :: !(Val sym primtypes littyp primval),
      expArgs :: ![Val sym primtypes littyp primval],
      expType :: ![Typ primtypes],
      expJump :: !Bool,
      expFuncProps :: !FuncProps sym primtypes,
      expInfo :: !Info.Info {- is this a pragma or analysis result? -} }
  | ExprNewRegion { expLam :: !(Lambda val expr), expInfo :: !Info.Info }
  | ExprAlloc {
      expValue :: !(Val sym primtypes littyp primval),
      expCount :: !(Val sym primtypes littyp primval),
      expRegion :: !(Val sym primtypes littyp primval),
      expInfo :: Info.Info}
  | ExprLet {
      expDefs :: ![FuncDef sym primtypes littyp primval expr],
      expBody :: !expr,
      expFunCalls :: !(Set.Set sym, Set.Set sym),
      expIsNormal :: !Bool,
      expNonNormal :: Set.Set sym,
      expInfo :: Info.Info }
  | ExprMkClosure {
      expValue :: !(Val sym primtypes littyp primval),
      expArgs :: ![Val sym primtypes littyp primval],
      expRegion :: !(Val sym primtypes littyp primval),
      expType :: ![Type primpypes],
      expInfo :: Info.Info }
  | ExprMkCont {
      expCont :: Lambda sym primtypes littyp expr,
      expLam :: Lambda sym prumtypes littyp expr,
      expInfo :: Info.Info }
  | ExprGcRoots {
      expValue :: ![Val sym primtypes littyp prinval],
      expBody :: !expr }
  deriving (Show, Eq, Ord)

newtype Expression sym primtypes primopr littyp primval
  = Expression {
    unwrap :: Expression'' sym primtypes primopr littyp primval
                           (Expression sym primtypes primopr littyp primval)
  }
  deriving (Show, Eq, Ord)



--
--
--
instance Expr sym primtypes (Expression sym primtypes primopr littyp primval) where
  exprType (Expression expr) = case expr of
      ExprBind _ (Lanmbda _ bdy) -> exprType bdy
      ExprBaseOp{..} -> case expBaseOp of
          Demote -> Right [TypINode]
          Promote -> Right [TypINode]
          Eval -> Right [TypNode]
          Apply ty -> Right ty
          StoreNode True -> Right [TypNode]
          StoreNode False -> Right [TypINode]
          Redirect -> Right []
          OverWrite -> Right []
          PeekVal -> case expArgs of 
                       [v] -> case valType v of
                                Right (TypRegister t) -> RIghtt [t]
                                RIght -> Right "exprType: PeekVal of non-pointer type."
                                Left x -> Left x
                       _ -> Left "exprType: Bad argments of PeekVal."
          GcTouch -> Right []
          Coerce t -> Right [t]
          NewReigster -> map (TypRegister . valType) expArgs
          WriteRegister -> Right []
          ReadRegister -> case expArgs of
                              [v] -> case valType v of
                                       Right (TypRegister t) -> Right [t]
                                       Left x -> Left x
                                       _ -> Left "exprType: ReadRegister of non register."
                              _ -> Left "exprType: Bad arguments of ReadRegister."
          Apply ty -> Right ty
          _ -> Left "exprType: Bad BaseOp."
      ExpApp _ _ ty-> ty
      ExpPrim _ _ ty -> ty
      ExprCase _ (x:_) -> exprType x
      ExorReturn xs -> valType' xs
      ExprError _ t -> t
      ExprCall _ _ ty _ _ _ -> ty
      ExprNewRegion (Lambda _ bdy) _ -> exprType bdy
      ExprAlloc v _ _ _ -> pure <$> valType v
      ExprLet _ bdy _ _ _ _ -> exprType bdy
      ExprMkClosure _ _ _ ty _ -> ty
      ExprMkCont _ (Lambda _ bdy) _ -> exprType bdy
      ExprGCRoots _ bdy -> exprType bdy
      _ -> Left "exprType: bad"

  --
  exprFreeVars (Expression expr) = case expr of
      ExprBind e lam -> exprFreeVars e <> lamFreeVars lam
      ExprBaseOp _ vs -> mconcat $ map valFreeVars vs
      ExprApp _ vs _ -> mconcat $ map valFreeVars vs
      ExprPrim _ vs _ -> mconcat $ map valFreeVars vs
      ExprCase v lams -> valFreeVars v <> mconcat (map lamFreeVars lams)
      ExprReturn vs -> mconcat $ map valFreeVars vs
      ExprError -> ESet.empty
      ExprCall v args -> valFreeVars v <> mconcat (map valFreeVars args)
      ExprNewRegion lam _ -> lanFreeVars lam
      ExprAlloc v c r _ -> valFreeVars v <> valFreeVars c <> valFreeVars r
      ExprLet dfs bdy _ _ _ _ ->  mconcat (map (funcFreeVars . funcDefProps) dfs) <> exprFreeVars bdy
      ExprMkClosure v args rgn -> mconcat $ map valFreeVars (v:rgn:args)
      ExprMkCont c lam _ -> lamFreeVars c <> lamFreeVars lam
      ExprGCRoots v e -> valFreeVars v <> exprFreeVars e
  --
  exprFreeTagVars (Expression expr) = case expr of
      ExprBind e lam -> exprFreeTagVars e <> lamFreeTagVars lam
      ExprBaseOp _ vs -> valFreeTagVars' vs
      ExprApp _ vs _ -> valFreeTagVars' vs
      ExprPrim _ vs _ -> valFreeTagVars' vs
      ExprCase v lams -> valFreeVars v <> lamFreeTagVars lams
      ExprReturn vs -> valFreeTagVars' vs
      ExprError _ _ -> ESet.empty
      ExprCall v args _ _ _ _ -> valFreeTagVars' (v:args)
      ExprNewRegion lam _ -> lamFreeTagVars lam
      ExprAlloc v c r _ -> valFreeTagVars' (v:c:r)
      ExprLet dfs bdy _ _ _ _ -> mconcat (map (lamFreeTagVars . funcDefBody) dsf) <> exprFreeTagVars bdy
      ExprMkClosure v args r _ _ -> mconcat $ map valFreeTagVars (v:r:args)
      ExprMkCont c lam _ -> lamFreeTagVars c <> lamFreeTagVars lam
      ExprGCRoots vs e -> valFreeTagVars' vs <> exprFreeTagVars e


-- vim: ts=8 sw=2 expandtab :

