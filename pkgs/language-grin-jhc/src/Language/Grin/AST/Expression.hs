module Language.Grin.AST.Expression (
  FuncDef(..),
  --
  Expression''(..),
  exprFreeVars, exprFreeTagVars, exprType,
  --
  lamType, lamFreeVars,
  --
  exprIsNop, exprIsOmittable, exprIsErrOmittable,
  --
  Expression(..),
  ) where

import Data.Bool (bool)
import qualified Data.Text as T
import qualified Data.Set as Set

import qualified Data.EnumSet.EnumSet as ESet -- containers-missing

import Text.PrettyPrint.ANSI.Leijen hiding((<$>), bool)

import Language.Grin.AST.Tag
import Language.Grin.AST.Var
import Language.Grin.AST.Val
import Language.Grin.AST.Type
import Language.Grin.AST.Lambda
import Language.Grin.AST.BasicOperation
import Language.Grin.Data.FuncProps
import Language.Grin.Data.FuncDef
import Language.Grin.Internal.Classes
import Language.Grin.Internal.Classes.PrimOpr
import qualified Language.Grin.Internal.Highlighter as H


-- ---------------------------------------------------------------------------


data Expression'' sym primtypes primopr primval expr
  = ExprBind !expr !(Lambda expr)
  | ExprBaseOp {
      expBaseOp :: !(BasicOperation primtypes),
      expArgs :: ![Val sym primtypes primval] }
  | ExprApp {
      expFunction :: !(Tag sym),
      expArgs :: ![Val sym primtypes primval],
      expType :: ![Typ primtypes] }
  | ExprPrim {
      expPrimitive :: primopr,
      expArgs :: ![Val sym primtypes primval],
      expType :: ![Typ primtypes] }
  | ExprCase {
      expValue :: !(Val sym primtypes primval),
      expAlts :: ![Lambda expr] }
  | ExprReturn {
      expValues :: ![Val sym primtypes primval] }
  | ExprError {
      expError :: T.Text,
      expType :: ![Typ primtypes] }
  | ExprCall {
      expValue :: !(Val sym primtypes primval),
      expArgs :: ![Val sym primtypes primval],
      expType :: ![Typ primtypes],
      expJump :: !Bool,
      expFuncProps :: !(FuncProps sym primtypes)
      {- expInfo :: !Info.Info -} {- is this a pragma or analysis result? -} }
  | ExprNewRegion {
      expLam :: !(Lambda expr)
      {-, expInfo :: !Info.Info -} }
  | ExprAlloc {
      expValue :: !(Val sym primtypes primval),
      expCount :: !(Val sym primtypes primval),
      expRegion :: !(Val sym primtypes primval)
      {- expInfo :: Info.Info -} }
  | ExprLet {
      expDefs :: ![FuncDef sym primtypes primval expr],
      expBody :: !expr,
      expFunCalls :: !(Set.Set sym, Set.Set sym),
      expIsNormal :: !Bool,
      expNonNormal :: !(Set.Set sym)
      {- expInfo :: Info.Info -} }
  | ExprMkClosure {
      expValue :: !(Val sym primtypes primval),
      expArgs :: ![Val sym primtypes primval],
      expRegion :: !(Val sym primtypes primval),
      expType :: ![Typ primtypes]
      {- expInfo :: Info.Info  -} }
  | ExprMkCont {
      expCont :: !(Lambda expr),
      expLam :: !(Lambda expr)
      {- expInfo :: Info.Info -} }
  | ExprGcRoots {
      expValues :: ![Val sym primtypes primval],
      expBody :: !expr }
  -- deriving (Show, Eq, Ord)



-- ---------------------------------------------------------------------------


instance (Expr expr,
          sym ~ ExprSym expr, primtypes ~ ExprPrimTypes expr, primval ~ ExprPrimVal expr,
          primopr ~ ExprPrimOpr expr,
          Expression'' ~ ExprRep expr,
          Pretty sym,
          Pretty (Val sym primtypes primval),
          Pretty primtypes,
          Pretty primval,
          PrimOpr primopr, PrimType primtypes, Pretty expr )
    => Pretty (Expression'' sym primtypes primopr primval expr) where
  pretty = go empty where
    -- highlighter
    prettyVals vs = case vs of
                  [v] -> pretty v
                  _ -> tupled $ map pretty vs
    applyKwd k vs = H.kwd k <+> hsep (map pretty vs)
    applyKwd' k vs = H.kwd k <+> tupled (map pretty vs)
    --
    larr = H.opr "<-"
    --
    -- go :: Doc -> Expression'' sym primtypes primopr primval expr -> Doc
    go vl = \case
      ExprBind e1 (Lambda vs e2) ->
        case exprUnwrap e1 of
          ExprBind{} -> align $ tupled (map pretty vs) <+> larr <+> go empty (exprUnwrap e1)
                                 <> line <> go vl (exprUnwrap e2)
          _ -> align $ go (if null vs
                              then empty
                              else tupled (map pretty vs) <+> larr) (exprUnwrap e1)
                       <> line <> go vl (exprUnwrap e2)
      ExprBaseOp Demote vs -> vl <+> applyKwd "demote" vs
      ExprBaseOp (StoreNode b) vs
        -> vl <+> H.kwd (bool "istore" "dstore" b)
              <+> case vs of
                    [x] -> pretty x
                    [x, y] -> pretty x <> char '@' <> pretty y
                    _ -> error "pretty(Expr): Bad StoreNode opr."
      -- ExprBaseOp Consume -> error
      ExprBaseOp GcTouch vs       -> vl <+> applyKwd' "gcTOuch" vs
      ExprBaseOp GcPush vs        -> vl <+> applyKwd' "gcPush" vs
      ExprBaseOp NewRegister vs   -> vl <+> applyKwd' "register" vs
      ExprBaseOp ReadRegister [r] -> vl <+> H.kwd "*" <> pretty r
      ExprBaseOp WriteRegister [r, x] -> vl <+> pretty r <+> H.kwd ":=" <+> pretty x
      ExprBaseOp Promote vs       -> vl <+> applyKwd "promote" vs
      ExprBaseOp Eval vs          -> vl <+> applyKwd "eval" vs
      ExprBaseOp (Apply _) vs     -> vl <+> applyKwd "apply" vs
      ExprBaseOp Redirect vs      -> vl <+> applyKwd "redirect" vs
      ExprBaseOp Overwrite vs     -> vl <+> applyKwd "overwrite" vs
      ExprBaseOp (Coerce _) vs    -> vl <+> applyKwd "coerce" vs
      ExprBaseOp PokeVal vs       -> vl <+> applyKwd "pokeval" vs
      ExprBaseOp PeekVal vs       -> vl <+> applyKwd "peekval" vs
      ExprApp s vs _ -> vl <+> H.fncname (pretty s) <+> hsep (map pretty vs)
      ExprPrim{..} -> prettyPrimOpr expPrimitive expArgs expType
      ExprCase v vs -> vl <+> H.kwd "case" <+> pretty v <+> H.kwd "of" <> line
                       <> indent 2 (vsep (map f vs))
        where
          f (Lambda [v'] e) =
            pretty v' <+> H.opr "->" <+> case exprUnwrap e of
                    ExprBind{}   -> H.kwd "do" <+> align (pretty e)
                    ExprCase{}   -> H.kwd "do" <+> align (pretty e)
                    ExprLet{}    -> H.kwd "do" <+> align (pretty e)
                    ExprMkCont{} -> H.kwd "do" <+> align (pretty e)
                    _ -> pretty e
      ExprReturn vs -> vl <+> H.kwd "return" <+> case vs of
                    [v] -> pretty v
                    xs -> tupled (map pretty xs)
      ExprError s _ -> if T.null s
                          then H.prim "exitFailure"
                          else H.kwd "error" <+> text (T.unpack s)
      ExprCall v args _ jmps _ ->
        vl <> (case v of
                 ValItem s (TypCall c _ _)
                   | c == Function || c == LocalFunction
                      -> text (if jmps then "jump to" else "call") <+> H.fncname (pretty s)
                 ValVar v' (TypCall c _ _) -> f jmps c <+> pretty v'
                   where f False Continuation = "cut to"
                         f False Function = "call"
                         f True Function = "jump to"
                         f False Closure = "enter"
                         f True Closure = "jump into"
                         f x y = pretty $ show (x, y)
                 ValPrim ap [] (TypCall Primitive' _ _) -> H.prim (pretty ap)
                 _ -> error "pretty(Expr): Bad call")
           <+> hsep (map pretty args)
      ExprNewRegion (Lambda r bdy) ->
        vl <> H.kwd "region" <+> "\\" <> prettyVals r <+> "->"
           <+> H.kwd "do" <+> align (pretty bdy)
      ExprAlloc{..} -> vl <+> H.kwd "alloc" <+> pretty expValue
                          <+> case expCount of
                                ValLit n _ | n == 1 -> empty
                                _ -> brackets (pretty expCount)
                          <+> "at" <+> pretty expRegion
      ExprLet{..} -> vl <+> H.kwd (if expIsNormal then "let" else "let*")
                     <> align (vsep $ map pretty expDefs)
                     <> line <> "in" <> align (pretty expBody)
      -- ExprMkClosure -> error
      -- ExprMkCont -> error
      ExprGcRoots vs bdy -> vl <+> H.kwd "withRoots" <> tupled (map pretty vs)
                            <+> line <> indent 2 (pretty bdy)
      _ -> error "pretty(Expr): Bad expression"




-- ---------------------------------------------------------------------------


exprFreeVars :: (Expr e, Expression'' ~ ExprRep e) => e -> ESet.EnumSet Var
exprFreeVars expr = case exprUnwrap expr of
  ExprBind e lam -> exprFreeVars e <> lamFreeVars lam
  ExprBaseOp _ vs -> valFreeVars vs
  ExprApp _ vs _ -> valFreeVars vs
  ExprPrim _ vs _ -> valFreeVars vs
  ExprCase v lams -> ESet.unions $ valFreeVars [v] : map lamFreeVars lams
  ExprReturn vs -> valFreeVars vs
  ExprError{} -> ESet.empty
  ExprCall v args _ _ _ -> valFreeVars $ v:args
  ExprNewRegion lam -> lamFreeVars lam
  ExprAlloc v c r -> valFreeVars [v,c,r]
  ExprLet dfs bdy _ _ _ ->  mconcat (map (funcFreeVars . funcDefProps) dfs) <> exprFreeVars bdy
  ExprMkClosure v args rgn _ -> valFreeVars (v:rgn:args)
  ExprMkCont c lam -> lamFreeVars c <> lamFreeVars lam
  ExprGcRoots v e -> valFreeVars v <> exprFreeVars e




exprFreeTagVars :: (Ord (ExprSym e), Expr e, Expression'' ~ ExprRep e)
                => e -> Set.Set (Tag (ExprSym e))
exprFreeTagVars expr = case exprUnwrap expr of
  ExprBind e lam -> exprFreeTagVars e <> lamFreeTagVars lam
  ExprBaseOp _ vs -> valFreeTagVars vs
  ExprApp _ vs _ -> valFreeTagVars vs
  ExprPrim _ vs _ -> valFreeTagVars vs
  ExprCase v lams -> Set.unions $ valFreeTagVars [v] : map lamFreeTagVars lams
  ExprReturn vs -> valFreeTagVars vs
  ExprError _ _ -> Set.empty
  ExprCall v args _ _ _ -> valFreeTagVars (v:args)
  ExprNewRegion lam -> lamFreeTagVars lam
  ExprAlloc v c r -> valFreeTagVars [v, c, r]
  ExprLet dfs bdy _ _ _ -> mconcat (map (lamFreeTagVars . funcDefBody) dfs) <> exprFreeTagVars bdy
  ExprMkClosure v args rgn _ -> valFreeTagVars (v:rgn:args)
  ExprMkCont c lam -> lamFreeTagVars c <> lamFreeTagVars lam
  ExprGcRoots vs e -> valFreeTagVars vs <> exprFreeTagVars e






exprType :: (Monad m, Expr e, Expression'' ~ ExprRep e)
         => e -> m [Typ (ExprPrimTypes e)]
exprType expr = case exprUnwrap expr of
  ExprBind _ (Lambda _ bdy) -> exprType bdy
  ExprBaseOp{..} -> case expBaseOp of
      Demote   -> return [TypINode]
      Promote  -> return [TypINode]
      Eval     -> return [TypNode]
      Apply ty -> return ty
      StoreNode True -> return [TypNode]
      StoreNode False -> return [TypINode]
      Redirect -> return []
      Overwrite -> return []
      PeekVal -> case expArgs of 
                   [v] -> valType v >>= \case
                            TypRegister t -> return [t]
                            _ -> fail "exprType: PeekVal of non-pointer type."
                   _ -> fail "exprType: Bad argments of PeekVal."
      GcTouch -> return []
      Coerce t -> return [t]
      NewRegister -> mapM (\x -> TypRegister <$> valType x) expArgs
      WriteRegister -> return []
      ReadRegister -> case expArgs of
                        [v] -> valType v >>= \case
                            TypRegister t -> return [t]
                            _ -> fail "exprType: ReadRegister of non register."
                        _ -> fail "exprType: Bad arguments of ReadRegister."
      _ -> fail "exprType: Bad BaseOp."
  ExprApp _ _ ty-> return ty
  ExprPrim _ _ ty -> return ty
  ExprCase _ (Lambda _ x:_) -> exprType x
  ExprReturn xs -> mapM valType xs
  ExprError _ t -> return t
  ExprCall _ _ ty _ _ -> return ty
  ExprNewRegion (Lambda _ bdy) -> exprType bdy
  ExprAlloc v _ _ -> pure <$> valType v
  ExprLet _ bdy _ _ _ -> exprType bdy
  ExprMkClosure _ _ _ ty -> return ty
  ExprMkCont _ (Lambda _ bdy) -> exprType bdy
  ExprGcRoots _ bdy -> exprType bdy
  _ -> fail "exprType: bad"



-- ---------------------------------------------------------------------------


lamType :: (Expr expr, Expression'' ~ ExprRep expr)
        => Lambda expr -> Either T.Text ([Typ (ExprPrimTypes expr)], [Typ (ExprPrimTypes expr)])
lamType Lambda{..} =
  case (mapM valType lamBind, exprType lamExpr) of
    (Left x, _) -> Left x
    (_, Left x) -> Left x
    (Right x, Right y) -> Right (x, y)



lamFreeVars :: (Expr expr, Expression'' ~ ExprRep expr) => Lambda expr -> ESet.EnumSet Var
lamFreeVars Lambda{..} = ESet.intersection (exprFreeVars lamExpr) (valFreeVars lamBind)



lamFreeTagVars :: (Expr expr, Expression'' ~ ExprRep expr, Ord (ExprSym expr))
               => Lambda expr -> Set.Set (Tag (ExprSym expr))
lamFreeTagVars (Lambda vs e) = Set.intersection (exprFreeTagVars e) (valFreeTagVars vs)




-- ---------------------------------------------------------------------------


exprIsNop :: (Expr expr, Expression'' ~ ExprRep expr) => expr -> Bool
exprIsNop e = case exprUnwrap e of
  ExprBaseOp Promote _ -> True
  ExprBaseOp Demote _ -> True
  _ -> False



exprIsOmittable :: (Expr expr, Expression'' ~ ExprRep expr,
                    PrimOpr (ExprPrimOpr expr))
                => expr -> Bool
exprIsOmittable e = case exprUnwrap e of
  ExprBaseOp Promote _ -> True
  ExprBaseOp Demote _ -> True
  ExprBaseOp PeekVal _ -> True
  ExprBaseOp ReadRegister _ -> True
  ExprBaseOp NewRegister _ -> True
  ExprBaseOp GcPush _ -> True
  ExprBaseOp (StoreNode _) _ -> True
  ExprAlloc{} -> True
  ExprReturn{} -> True
  ExprPrim{..} -> primCheap expPrimitive
  ExprCase{..} -> all exprIsOmittable $ map lamExpr expAlts
  ExprLet{..} -> exprIsOmittable expBody
  ExprBind e1 (Lambda _ e2) -> exprIsOmittable e1 && exprIsOmittable e2
  _ -> False



exprIsErrOmittable :: (Expr expr, Expression'' ~ ExprRep expr,
                       PrimOpr (ExprPrimOpr expr))
                   => expr -> Bool
exprIsErrOmittable e = case exprUnwrap e of
  ExprBaseOp Overwrite _ -> True
  ExprBaseOp PokeVal _   -> True
  ExprBaseOp WriteRegister _ -> True
  ExprBind e1 (Lambda _ e2) -> exprIsErrOmittable e1 && exprIsErrOmittable e2
  ExprCase _ as -> all exprIsErrOmittable $ map lamExpr as
  _ -> exprIsOmittable e






-- ---------------------------------------------------------------------------

newtype Expression sym primtypes primopr primval
  = Expression (
    Expression'' sym primtypes primopr primval
                 (Expression sym primtypes primopr primval) )
  -- deriving (Show, Eq, Ord)

instance (PrimType primtypes, Pretty sym, Pretty primtypes, Pretty primval, PrimOpr primopr)
    => Pretty (Expression sym primtypes primopr primval) where
  pretty (Expression x) = pretty x

instance Expr (Expression sym primtypes primopr primval) where
  type ExprSym       (Expression sym primtypes primopr primval) = sym
  type ExprPrimTypes (Expression sym primtypes primopr primval) = primtypes
  type ExprPrimOpr   (Expression sym primtypes primopr primval) = primopr
  type ExprPrimVal   (Expression sym primtypes primopr primval) = primval
  type ExprRep       (Expression sym primtypes primopr primval) = Expression''
  exprUnwrap (Expression x) = x
  exprWrap = Expression



-- vim: ts=8 sw=2 expandtab :

