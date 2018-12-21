module Language.Grin.Internal.Classes (
  HasType(..), HasFreeVars(..)
  ) where

import qualified Data.EnumSet.EnumSet as ESet
import qualified Data.Set as Set

import Language.Grin.AST.Tag
import Language.Grin.AST.Var

import qualified Data.Text as T
import Language.Grin.AST.Type

-- ---------------------------------------------------------------------------

class Expr sym primtypes expr | expr -> sym | expr -> primtypes where
  exprFreeVars :: expr -> (Set.EnumSet Var, Set (Tag sym))
  exprType :: expr -> Either T.Text [Typ primtypes]


-- vim : ts=2 sw=2 expandtab :

