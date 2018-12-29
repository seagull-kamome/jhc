module Language.Grin.Data.FFIExport (
  Safety(..), CallConv(..), FFIExport(..)
  ) where

import Data.Typeable

data Safety = Safe | Unsafe deriving(Eq, Ord, Show)
data CallConv = CCall | StdCall | CApi | Primitive | DotNet
  deriving(Eq,Ord,Show)


{-
data FFIType sym
  = Import sym Requires
  | ImportAddr sym Requires
  | Wrapper
  | Dynamic
  deriving (Eq, Ord, Show)



data FFISpec = FFISpec FFIType Safety CallConv
             deriving(Eq,Ord,Show)
-}


data FFIExport sym = FFIExport {
  ffiExportCName    :: sym,
  ffiExportSafety   :: Safety,
  ffiExportCallConv :: CallConv,
  ffiExportArgTypes :: [sym],
  ffiExportRetType  :: sym
  }
  deriving(Eq, Ord, Show, Typeable)



