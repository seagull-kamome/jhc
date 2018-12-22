module Language.Grin.AST.Var (
  Var(..), v0, v1, v2, v3
  ) where

import Text.PrettyPrint.ANSI.Leijen hiding((<$>))

-- ---------------------------------------------------------------------------

newtype Var = Var Int deriving (Show, Eq, Ord, Enum)

instance Pretty Var where
  pretty (Var n) = char 'v' <> int n


-- ---------------------------------------------------------------------------

v0, v1, v2, v3 :: Var
v0 = Var 0
v1 = Var 1
v2 = Var 2
v3 = Var 3


-- vim: ts=8 sw=2 expandtab :


