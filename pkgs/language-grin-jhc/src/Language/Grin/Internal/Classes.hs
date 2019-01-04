module Language.Grin.Internal.Classes (
  Expr(..), PrimType(..)
  ) where

import qualified Data.Text as T
import Text.PrettyPrint.ANSI.Leijen hiding((<$>), bool)

-- ---------------------------------------------------------------------------

class PrimType primtypes where
  varPrefix :: primtypes -> T.Text


class (Ord (ExprSym e), PrimType (ExprPrimTypes e)) => Expr e where
  type ExprSym e :: *
  type ExprPrimTypes e :: *
  type ExprPrimOpr e :: *
  type ExprPrimVal e :: *
  type ExprRep e :: * -> * -> * -> * -> * -> *
  exprUnwrap :: e -> ExprRep e (ExprSym e) (ExprPrimTypes e) (ExprPrimOpr e) (ExprPrimVal e) e
  exprWrap :: ExprRep e (ExprSym e) (ExprPrimTypes e) (ExprPrimOpr e) (ExprPrimVal e) e -> e


-- vim : ts=2 sw=2 expandtab :

