{-# LANGUAGE UndecidableInstances #-}
module Language.Grin.AST.Lambda (Lambda(..)) where

import Language.Grin.AST.Val
import Language.Grin.Internal.Classes
import qualified Language.Grin.Internal.Highlighter as H

import Text.PrettyPrint.ANSI.Leijen hiding((<$>))

-- ---------------------------------------------------------------------------

data Lambda expr
  = Lambda {
    lamBind :: ![Val (ExprSym expr) (ExprPrimTypes expr) (ExprPrimVal expr)],
    lamExpr :: ! expr }

instance (Expr expr, PrimType primtypes,
          sym ~ ExprSym expr, primtypes ~ ExprPrimTypes expr,
          primval ~ ExprPrimVal expr,
          Pretty sym, Pretty primval, Pretty primtypes, Pretty expr)
         => Pretty (Lambda expr) where
  pretty Lambda{..} = 
    (if null lamBind
        then "()"
        else tupled (map pretty lamBind) <+> H.opr "->") <+> pretty lamExpr



-- vim: ts=8 sw=2 expandtab :

