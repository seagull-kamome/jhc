module Language.Grin.Internal.Classes.PrimOpr (
  PrimOpr(..)
  ) where


import Language.Grin.AST.Val
import Language.Grin.AST.Type

import Text.PrettyPrint.ANSI.Leijen hiding((<$>))

class PrimOpr primopr where
  prettyPrimOpr :: (Pretty (Val sym primtypes primval),
                    Pretty (Typ primtypes))
                => primopr
                -> [Val sym primtypes primval]
                -> [Typ primtypes] -> Doc
  primCheap :: primopr -> Bool


-- vim: ts=8 sw=2 expandtab :

