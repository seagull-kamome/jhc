module Language.Grin.Internal.Highlighter (
  opr, kwd, fncname, prim,
  opr', kwd', fncname', prim'
  ) where

import Text.PrettyPrint.ANSI.Leijen hiding((<$>))

import GHC.Exts (IsString(..))



-- ---------------------------------------------------------------------------

opr, kwd, fncname, prim :: Doc -> Doc
opr = id
kwd = id
fncname = id
prim = id


opr', kwd', fncname', prim' :: IsString str => str -> Doc
opr' = text . toString
kwd' = text . toString
fncname' = text . toString
prim' = text . toString


