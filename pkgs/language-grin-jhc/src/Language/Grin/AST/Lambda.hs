module Language.Grin.AST.Lambda (
  Lambda(..), lamType, lamFreeVars, lamFreeTagVars
  ) where

import qualified Data.Text as T
import qualified Data.EnumSet.EnumSet as ESet

import Language.Grin.AST.Tag
import Language.Grin.AST.Var
import Language.Grin.AST.Val
import Language.Grin.AST.Type
import Language.Grin.Internal.Classes

-- ---------------------------------------------------------------------------

data Lambda sym primtypes primval expr
  = Lambda {
    lamBind :: ![Val sym primtypes primval],
    lamExpr :: ! expr }


-- ---------------------------------------------------------------------------

lamType :: Expr expr e''
        => Lambda sym primtypes primval expr
        -> Either T.Text ([Typ primtypes], [Typ primtypes])
lamType Lambda{..} =
  case (go [] $ map valType lamBind, exprType lamExpr) of
    (Left x, _) -> Left x
    (_, Left x) -> Left x
    (Right x, Right y) -> Right (x, y)
  where
    go r [] = r
    go r (Right x:xs) = go (r:x) xs
    go r (Left x:_) = Left x



lamFreeVars :: Lambda sym primtypes primval expr -> ESet.EnumSet Var
lamFreeVars Lambda{..} = ESet.intersection (exprFreeVars lamExr) (mconcat $ map valFreeVars lamBind)



lamFreeTagVars :: Lambda sym primtypes primval expr -> ESet.EnumSet (Tag sym)
lamFreeTagVars (Lambda vs e) = ESet.intersection (exprFreeTagVars e) (valFreeTagVars' vs)


-- vim: ts=8 sw=2 expandtab :

