module Language.Grin.Data.FuncProps (
  FUncProps(..), newFuncProps, updateFuncProps
  ) where

import qualified Data.EnumSet.EnumSet as ESet
import qualified Data.Set as Set

import Language.Grin.AST.Var
import Language.Grin.AST.Val
import Language.Grin.AST.Tag
import Language.Grin.AST.Lambda
import Language.Grin.AST.Type
import Language.Grin.Data.Perhaps
import Language.Grin.Internal.Classes

-- ---------------------------------------------------------------------------

data FuncProps sym primtypes = FuncProps {
  funcInfo :: !Info.Info,
  funcFeeVars :: !ESet.EnumSet Var
  funcTags :: !Set.Set (Tag sym),
  funcType :: !([Typ primtypes], [Typ primtypes]),
  funcCuts :: Perhaps,
  funcAllocs :: Perhaps,
  funcCreates :: Perhaps,
  funcLoops :: Perhaps
  }
  deriving (Show, Eq, Ord)


-- ---------------------------------------------------------------------------


newFuncProps :: Expr sym primtypes expr
                => Lambda sym primtypes littyp expr
                -> FuncProps sym primtypes
newFuncProps lam@Lambda{..} = FuncProps {
    funcInfo = mempty,
    funcFeeVars = fv,
    funcTags = ft,
    funcType = lamType lam,
    funcCuts = Maybe,
    funcAllocs = Maybe,
    funcCreates = Maybe,
    funcLoops = Maybe }
  where (fv, ft) = exprFreeVars lamExp




updateFuncProps :: Expr sym primtypes expr
                => Lambda sym primtypes littyp expr
                -> FuncProps sym primtypes -> FuncProps sym primtypes
updateFuncProps lam@Lambda{..} fp@FuncProps{..} =
  fp { funcFeeVars = fv,
       funcTags = ft,
       funcType = lamType lam }
  where (fv, ft) = exprFreeVars lamExp



-- vim: ts=8 sw=2 expandtab :


