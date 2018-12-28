module Language.Grin.AST.Lambda (Lambda(..)) where

import Language.Grin.AST.Val

-- ---------------------------------------------------------------------------

data Lambda sym primtypes primval expr
  = Lambda {
    lamBind :: ![Val sym primtypes primval],
    lamExpr :: ! expr }


-- vim: ts=8 sw=2 expandtab :

