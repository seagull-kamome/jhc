module Language.Grin.Internal.Highlighter (
  opr, kwd, fncname, typname, prim
  ) where

import Text.PrettyPrint.ANSI.Leijen hiding((<$>))


-- ---------------------------------------------------------------------------

opr, kwd, fncname, typname, prim :: Doc -> Doc
opr = id
kwd = id
fncname = id
typname = id
prim = id




