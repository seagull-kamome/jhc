module Language.Grin.Internal.Highlighter (
  operator, keyword, funcname, primitive
  ) where

import Text.PrettyPrint.ANSI.Leijen hiding((<$>))


-- ---------------------------------------------------------------------------

operator, keyword, funcname, primitive :: Doc -> Doc
operator = id
keyword = id
funcname = id
primitive = id
{-# INLINE operator #-}
{-# INLINE keyword #-}
{-# INLINE funcname #-}
{-# INLINE primitive #-}


