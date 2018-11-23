module Util.Inst() where

import Control.Applicative
import Data.Foldable(Foldable(..))
import Data.Traversable(Traversable(..), foldMapDefault, fmapDefault)

instance Functor ((,,) a b) where fmap = fmapDefault
instance Foldable ((,,) a b) where foldMap = foldMapDefault
instance Traversable  ((,,) a b) where traverse f (x,y,z) = (,,) x y <$> f z

