module Jhc.Data.Interned.Text.Binary (InternedText(..)) where

import qualified Data.Text as T
import Data.Interned
import Data.Binary
import qualified Data.Interned.Text as IT

import GHC.Exts (IsString(..))

newtype InternedText = InternedText { unwrap :: IT.InternedText }
  deriving (Eq, Ord, IsString, Show)

-- instance Uninternable InternedText where unintern = unintern . unwrap

instance Binary InternedText where
  put = put . unintern. unwrap
  get = InternedText . intern <$> get



