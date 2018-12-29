module Language.Grin.EvalInline(createEvalApply) where

import qualified Data.Text as T

import qualified Control.Monad.Memo as Memo  -- monad-memo
import Control.Monad.Fresh.Flat -- monad-freshid

import Language.Grin.AST.Tag
import Language.Grin.AST.Lambda
import Language.Grin.AST.Type
import Language.Grin.AST.Expression
import Language.Grin.AST.Program
import Language.Grin.Data.TypeEnv
import Language.Grin.Transform



-- ---------------------------------------------------------------------------

{- PRIVATE -}
createApply :: Typ primtypes -> [Typ primtypes] -> TypeEnv sym primtypes -> [Tag sym] -> Lambda sym primtypes primval expr
createApply argType retType te ts' =
  Lambda (if argType == TypUnit then [n1] else [n1, n2]) $
         exprWrap (if null cs
           then ExprError ("Empty Apply:)" <> T.pack (show ts)) retType
           else ExprCase n1 cs)
  where
    ts = sort ts'
    a2 = ValVar v2 argType
    cs = map f $ filter tagGood ts
      where
        tagGood t = case findTypeOfType t te of
          Just TypeOfType{ typThunk = TypeApp mt w } ->
             (Just argType == mt || (argType == TypUnit && isNothing mt)) && (snd <$> findArgsType te w) == Just retType
          _ -> False
    f t = Lambda [ValNodeC t vs] g
      where
        (ts,_) = runIdentity $ findArgsType te t
        vs = zipWith Var [3..] ts
        Just (n, fn) = tagUnfunction t
        a2s = if argType == TypUnit then [] else [a2]
        g | n == 1 =  ExprApp fn (vs ++ a2s) ty
          | n > 1 = ExprBaseOp (StoreNode True) [ValNodeC (partialTag fn (n - 1)) (vs ++ a2s)]
          | otherwise = error "createApply"
         where
            Just (_,ty) = findArgsType te fn



{-# NOINLINE createEvalApply #-}
createEvalApply :: (Monad m, MonadIO m) => Program -> m Program
createEvalApply prg@Program{..} = do
  --
  let g (ExprBaseOp (Apply ty) [fun]) =
          ExprApp <$> Memo.memo h (TyUnit, ty) <*> pure [fun] <*> pure ty
      g (ExprBaseOp (Apply ty) [fun,arg]) =
          ExprApp <$> Memo.memp h (valType arg, ty) <*> pure [fun, arg] <*> pure ty
      g x = mapExpExp g x
      h _ = TagBApply <$>> lift fresh
  (funcs, s) <- Memo.startRunMemoT $ flip runFreshT 0 $
      forM progFunctions $ \FuncDef{..} ->
        (funcDefName,) <$> lamTransformBody g funcDefBody
  --
  let tags = Set.toList $ ftags <> plads
      ftags = Set.unions $ exprFreeVars $ map (lamExp . funcDefBody) progFunctions
      plads = Set.fromList $ concatMap mplad (Set.toList ftags)
      mplad t | Just (n,tag) <- tagUnfunction t, n > 1 = t:mplad (tagToPartial tag (n - 1))
      mplad t = [t]
  let (apps, ntyenv) = unzip $ map cf $ Map.toList s
      cf ((targ, tret), name)  = (
           (name, createApply targ tret progTypeEnv tags),
           (name, emptyTypeOfType {
                tySlots = if targ == TyUnit then [TyNode] else [TyNode, targ],
                tyReturn = tret }))
  return $ prg {
    progFunctions = map (curry newFuncDef) (apps ++ funcs),
    progTypeEnv = TyEnv $ fromTypeEnv progTypeEnv <> Map.fromList ntyenv
    }


-- vim: ts=8 sw=2 expandtab :

