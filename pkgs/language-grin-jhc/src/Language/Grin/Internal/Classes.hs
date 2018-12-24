module Language.Grin.Internal.Classes (
  HasType(..), HasFreeVars(..)
  ) where

import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.EnumSet.EnumSet as ESet -- containers-missing

import Language.Grin.AST.Tag
import Language.Grin.AST.Var
import Language.Grin.AST.Type


-- ---------------------------------------------------------------------------

class Expr e (e'' :: * -> * -> * -> * -> * -> *) | e -> e'' where
  type ExprSym e :: *
  type ExprPrimTypes e :: *
  type ExprPrimOpr e :: *
  type ExprPrimval e :: *
  exprUnwrap :: e -> e'' (ExprSym e) (ExprPrimTypes e) (ExprPrimOpr e) (ExprPrimVal e) e



class PrimType primtypes where
  varPrefix :: primtypes -> T.Text


class PrimOpr primopr where
  prettyPrimOpr :: (Pretty (Val sym primtypes primval),
                    Pretty (Typ primtypes))
                => primopr
                -> [Val sym primtypes primval]
                -> [Typ primtypes] -> Doc




-- vim : ts=2 sw=2 expandtab :

