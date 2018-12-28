module Language.Grin.Internal.Classes (
  Expr(..), PrimType(..)
  ) where

import qualified Data.Text as T

-- ---------------------------------------------------------------------------

class Expr (e'' :: * -> * -> * -> * -> * -> *) e | e -> e'' where
  type ExprSym e :: *
  type ExprPrimTypes e :: *
  type ExprPrimOpr e :: *
  type ExprPrimVal e :: *
  exprUnwrap :: e -> e'' (ExprSym e) (ExprPrimTypes e) (ExprPrimOpr e) (ExprPrimVal e) e
  exprWrap :: e'' (ExprSym e) (ExprPrimTypes e) (ExprPrimOpr e) (ExprPrimVal e) e-> e





class PrimType primtypes where
  varPrefix :: primtypes -> T.Text


-- vim : ts=2 sw=2 expandtab :

