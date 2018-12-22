module Language.Grin.Internal.Classes (
  HasType(..), HasFreeVars(..)
  ) where

import qualified Data.EnumSet.EnumSet as ESet -- containers-missing
import qualified Data.Set as Set

import Language.Grin.AST.Tag
import Language.Grin.AST.Var

import qualified Data.Text as T
import Language.Grin.AST.Type


-- ---------------------------------------------------------------------------

class Expr sym primtypes expr | expr -> sym, expr -> primtypes where
  exprFreeVars :: expr -> Set.EnumSet Var
  exprFreeTagVars :: expr -> Set (Tag sym)
  exprType :: Monad m => expr -> m [Typ primtypes]


class PrimType primtypes where
  varPrefix :: primtypes -> T.Text


-- vim : ts=2 sw=2 expandtab :

