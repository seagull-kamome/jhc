module Language.Grin.AST.Lambda (Lambda(..)) where

import Language.Grin.AST.Val
import Language.Grin.Internal.Classes

-- ---------------------------------------------------------------------------

data Lambda expr
  = Lambda {
    lamBind :: ![Val (ExprSym expr) (ExprPrimTypes expr) (ExprPrimVal expr)],
    lamExpr :: ! expr }


-- vim: ts=8 sw=2 expandtab :

