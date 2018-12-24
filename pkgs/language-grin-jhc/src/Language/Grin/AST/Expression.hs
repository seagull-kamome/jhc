module Language.Grin.AST.Expression (
  FuncDef(..), newFuncDef, updateFuncDef,
  --
  Expression''(..), Expression(..)
  ) where

import Data.Bool (bool)
import qualified Data.Text as T
import qualified Data.Set as Set

import qualified Data.EnumSet.EnumSet as ESet -- containers-missing

import Text.PrettyPrint.ANSI.Leijen ((<$>))

import Language.Grin.AST.Tag
import Language.Grin.AST.Var
import Language.Grin.AST.Type
import Language.Grin.AST.Lambda
import Language.Grin.AST.BasicOperation
import Language.Grin.Data.Perhaps
import Language.Grin.Data.FuncProp
import Language.Grin.Internal.Classes
import qualified Language.Grin.Internal.Highlighter as H


-- ---------------------------------------------------------------------------

data FuncDef sym primtypes primval expr = FuncDef {
    funcDefName :: !sym,
    funcDefBody :: !(Lambda sym primtypes expr),
    funcDefCall :: !(Val sym primtypes primval),
    funcDefProps :: !FuncProps sym (Typ primtypes)
  }
  deriving (Show, Eq, Ord)



newFuncDef :: Expr sym primtypes expr
           => Bool -> sym -> Lambda sym primtypes expr
           -> FuncDef sym primtypes primval expr
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
                   => Lambda sym primtypes primval expr
                   -> FuncDef sym primtypes primval expr
                   -> FuncDef sum primtypes primval expr
updateFuncDefProps lam fd@FuncDef{..} =
  fd { funcDefProps = updateFuncProps funcDefProps }




-- ---------------------------------------------------------------------------


data Expression'' sym primetypes primopr primval expr
  = ExprBind !expr !(Lambda (Val sym primtypes primval) expr)
  | ExprBaseOp {
      expBaseOp :: !(BasicOperation primtypes),
      expArgs :: ![Val sym primtypes primval] }
  | ExprApp {
      expFunction :: !sym,
      expArgs :: ![Val sym primtypes primval],
      expType :: ![Typ primtypes] }
  | ExprPrim {
      expPrimitive :: primopr,
      expArgs :: ![Val sym primtypes primval],
      expType :: ![Typ primtypes] }
  | ExprCase {
      expValue :: !(Val sym primtypes primval),
      expAlts :: ![Lambda sym primtypes expr] }
  | ExprReturn {
      expValues :: ![Val sym primtypes, primval] }
  | ExprError {
      expError :: T.Text,
      expType :: ![Typ primtypes] }
  | ExprCall {
      expValue :: !(Val sym primtypes primval),
      expArgs :: ![Val sym primtypes primval],
      expType :: ![Typ primtypes],
      expJump :: !Bool,
      expFuncProps :: !FuncProps sym primtypes,
      expInfo :: !Info.Info {- is this a pragma or analysis result? -} }
  | ExprNewRegion { expLam :: !(Lambda val expr), expInfo :: !Info.Info }
  | ExprAlloc {
      expValue :: !(Val sym primtypes primval),
      expCount :: !(Val sym primtypes primval),
      expRegion :: !(Val sym primtypes primval),
      expInfo :: Info.Info}
  | ExprLet {
      expDefs :: ![FuncDef sym primtypes primval expr],
      expBody :: !expr,
      expFunCalls :: !(Set.Set sym, Set.Set sym),
      expIsNormal :: !Bool,
      expNonNormal :: Set.Set sym,
      expInfo :: Info.Info }
  | ExprMkClosure {
      expValue :: !(Val sym primtypes primval),
      expArgs :: ![Val sym primtypes primval],
      expRegion :: !(Val sym primtypes primval),
      expType :: ![Type primpypes],
      expInfo :: Info.Info }
  | ExprMkCont {
      expCont :: Lambda sym primtypes expr,
      expLam :: Lambda sym prumtypes expr,
      expInfo :: Info.Info }
  | ExprGcRoots {
      expValue :: ![Val sym primtypes prinval],
      expBody :: !expr }
  deriving (Show, Eq, Ord)



-- ---------------------------------------------------------------------------


instance (Pretty expr,
          Pretty sym,
          Pretty (Val sym primtypes primval),
          PrimOpr primopr )
    => Pretty (Expression sym primtypes primptr primval expr) where
  pretty = go empty where
    -- highlighter
    kwd = H.keyword . text
    prettyVals k vs = case vs of
                  [v] -> pretty v
                  _ -> tupled $ map pretty vs
    applyKwd k vs = kwd k <+> hsep (map pretty vs)
    applyKwd' k vs = kwd k <+> tupled (map pretty vs)
    prim = H.primitive . text
    --
    larr = operator "<-"
    --
    go vl = \case
      ExprBind e1@ExprBind{} (Lambda vs e2)
        -> align $ tupled (map pretty vs) <+> larr <+> go empty e1
                   <> line <> go vl e2
      ExprBind e1 (Lambda vs w2)
        -> align $ go (if null vs
                          then empty
                          else tuples (map pretty vs) <+> larr) e1
                   <> line <> go vl e2
      ExprBaseOp Demote vs -> vl <+> applyKwd "demote" vs
      ExprBaseOp (StoreNode b) vs
        -> vl <+> H.keyword (text $ bool "istore" "dstore" b)
              <+> case vs of
                    [x] -> pretty x
                    [x, y] -> pretty x <> char '@' <> pretty y
                    _ -> error "pretty(Expr): Bad StoreNode opr."
      -- ExprBaseOp Consume -> error
      ExprBaseOp GCTouch vs       -> vl <+> applyKwd' "gcTOuch" vs
      ExprBaseOp GCPush vs        -> vl <+> applyKwd' "gcPush" vs
      ExprBaseOp NewRegister vs   -> vl <+> applyKwd' "register" vs
      ExprBaseOp ReadRegister [r] -> vl <+> kwd "*" <> pretty r
      ExprBaseOp WriteRegister [r, x] -> vl <+> pretty r <+> kwd ":=" <+> pretty x
      ExprBaseOp op vs            -> vl <+> applyKwd x vs
        where x = case op of
                    Promote -> "promote"
                    Eval -> "eval"
                    Apply -> "apply"
                    Redirect -> "redirect"
                    Overwrite -> "overwrite"
                    Coerce _ -> "coerce"
                    PokeVal -> "pokeVal"
                    PeekVal -> "peekval"
      ExprApp s vs _ -> vl <+> H.funcname (pretty s) <+> hsep (map pretty vs)
      ExprPrim{..} -> prettyPrimOpr exprPrimitive expArgs expType
      ExprCase v vs -> vl <+> kwd "case" <+> pretty v <+> kwd "ok" <> hline
                       <> indent 2 (vsep (map f vs))
        where
          f (Lambda [v] e) =
            pretty v <+> operator "->" <+> case e of
                    ExprBind{}   -> kwd "do" <+> align (pretty e)
                    ExprCase{}   -> kwd "do" <+> align (pretty e)
                    ExprLet{}    -> kwd "do" <+> align (pretty e)
                    ExprMkCOnt{} -> kwd "do" <+> align (pretty e)
                    _ -> pretty e
      ExprReturn vs -> vl <+> kwd "return" <+> case vs of
                    [v] -> pretty v
                    xs -> tupled (map pretty xs)
      ExprError s _ -> if null s
                          then prim "exitFailure"
                          else kwd "error" <+> text (T.unpack s)
      ExprCall v args _ jmps _ _ ->
        vl <> (case v of
                 ValItem s (TypCall c _ _)
                   | c == Function || c == LocalFUnction
                      -> text (if jmps then "jump to" else "call") <+> H.funcname (pretty s)
                 ValVar v' (TypCall c _ _) -> f jmps c <+> pretty v'
                   where f False Continuation = text "cut to"
                         f False Function = text "call"
                         f True Function = textt "jump to"
                         f False Closure = text "enter"
                         f True Closure = text "jump into"
                         f x y = pretty $ show (x, y)
                 ValPrim ap [] (TypCall Primitive' _ _) -> H.primitive (pretty ap)
                 _ -> error "pretty(Expr): Bad call")
           <+> hsep (map pretty args)
      ExprNewRegion (Lambda r bdy) _ ->
        vl <> keyword "region" <+> text "\\" <> prettyVals r <+> text "->"
           <+> kwd "do" <+> align (pretty e)
      ExprAlloc{..} -> vl <+> H.keyword (text "alloc")
                          <+> pretty expValue
                          <+> case expCount of
                                ValLit n _ | n == 1 -> empty
                                _ -> breckets (pretty expCount)
                          <+> text "at" <+> pretty expRegion
      ExprLet{..} -> vl <+> H.keyword (if exprIsNormal then "let" else "let*")
                     <> align (vsep $ map f exprDefs)
                     <> line <> text "in" <> align (pretty exprBody)
        where
          f FuncDef{..} = H.funcname (pretty funcDefName)
                            <+> hsep (map pretty $ lamBind fundcDefBody)
                            <+> H.operator (text "=")
                            <+> H.keyword (text "do")
                            <+> align (pretty $ lamExpr funcDefBody)
      -- ExprMkClosure -> error
      -- ExprMkCont -> error
      ExprGCRoots vs bdy -> vl <+> kwd "withRoots" <> tupled (map pretty vs)
                            <+> hline <> indent 2 (pretty b)
      _ -> error "pretty(Expr): Bad expression"




-- ---------------------------------------------------------------------------


exprFreeVars :: Expr e Expression'' => e -> Set.EnumSet Var
exprFreeVars expr = case exprUnwrap expr of
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




exprFreeVarTags :: Expr e Expression'' => e -> Set.Set (Tag (ExprSym e))
exprFreeTagVars expr = case exprUnwrap expr of
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






exprType :: (Monad m, Expr e Expression'') => e -> m (Typ (ExprPrimTypes e))
exprType expr = case exprUnwrap expr of
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




-- ---------------------------------------------------------------------------

newtype Expression sym primtypes primopr primval
  = Expression (
    Expression'' sym primtypes primopr primval
                 (Expression sym primtypes primopr primval) )
  deriving (Show, Eq, Ord)

instance Pretty (Expression sym primtypes primopr primval) where
  pretty (Expression x) = pretty x

instance Expr (Expression sym primtypes primopr primval) Expression'' where
  type ExprSym (Expression sym primtypes primopr primval) = sym
  type ExprPrimType (Expression sym primtypes primopr primval) = primtype
  type ExprPrimOpr (Expression sym primtypes primopr primval) = primopr
  type ExprPrimVal (Expression sym primtypes primopr primval) = primval
  exprUnwrap (Expression x) = x



-- vim: ts=8 sw=2 expandtab :

