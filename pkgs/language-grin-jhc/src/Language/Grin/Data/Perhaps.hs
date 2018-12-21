module Language.Grin.Data.Perhaps (Perhaps(..) ) where

data Perhaps = No | Maybe | Yes
  deriving (Show, Eq, Ord)


