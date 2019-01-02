module Language.Grin.Data.FuncDef (
  FuncDef(..)
  ) where

import Text.PrettyPrint.ANSI.Leijen hiding((<$>), bool)

import Language.Grin.AST.Tag
import Language.Grin.AST.Val
import Language.Grin.AST.Lambda
import Language.Grin.Data.FuncProps
import Language.Grin.Internal.Classes
import qualified Language.Grin.Internal.Highlighter as H


-- ---------------------------------------------------------------------------

data FuncDef sym primtypes primval expr = FuncDef {
    funcDefName  :: !(Tag sym),
    funcDefBody  :: !(Lambda expr),
    funcDefCall  :: !(Val sym primtypes primval),
    funcDefProps :: !(FuncProps sym primtypes)
  }
  -- deriving (Show, Eq, Ord)


instance (Expr expr,
          sym ~ ExprSym expr, primtypes ~ ExprPrimTypes expr,
          primval ~ ExprPrimVal expr,
          Pretty sym, PrimType primtypes, Pretty primtypes, Pretty primval,
          Pretty expr )
    => Pretty (FuncDef sym primtypes primval expr) where
  pretty FuncDef{..} = H.fncname (pretty funcDefName)
                   <+> hsep (map pretty $ lamBind funcDefBody)
                   <+> H.opr "=" <+> H.kwd "do"
                   <+> align (pretty $ lamExpr funcDefBody)


-- ---------------------------------------------------------------------------

-- vim: ts=8 sw=2 expandtab :

