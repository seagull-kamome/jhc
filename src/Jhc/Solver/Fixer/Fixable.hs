module Jhc.Solver.Fixer.Fixable (
  HasTop(..), HasBottom(..), Fixable(..), Topped(..)
  ) where

import Data.Maybe (isNothing)
import qualified Data.Set as Set
import GHC.Exts (IsString(..))

class HasBottom a where bottom :: a
class HasTop a where top :: a

-- ---------------------------------------------------------------------------
-- | Fixable class, must satisfy the following laws.
--
-- maybe True isBottom bottom = True
-- maybe True isTop top == True
-- x `join` x == x
-- x 'join` y == y `join` x
-- x `join` bottom == x
-- x `meet` bottom == x
-- bottom `meet` x = bottom
-- x `meet` y == z --> y `join` z = x
class Fixable a where
  -- determine if we are at the top or bottom of lattice.
  isBottom :: a -> Bool
  isTop :: a -> Bool

  -- operators
  join :: a -> a -> a
  meet :: a -> a -> a
  eq :: a -> a -> Bool
  lte :: a -> a -> Bool
  showFixable :: IsString str => a -> str

  -- default implements
  isBottom = const False
  {-# INLINE isBottom #-}
  isTop = const False
  {-# INLINE isTop #-}
  lte x y = isBottom (x `meet` y)
  {-# INLINE lte #-}
  eq x y = lte x y && lte y x
  {-# INLINE eq #-}
  showFixable x | isBottom x = fromString "B"
                | isTop x = fromString "T"
                | otherwise = fromString "*"


-- ---------------------------------------------------------------------------
-- Instances

instance Ord a => HasBottom (Set.Set a) where bottom = mempty
instance Ord a => Fixable (Set.Set a) where
  isBottom = Set.null
  join = Set.union
  meet = Set.intersection
  lte = Set.isSubsetOf
  eq = (==)


instance HasBottom Bool where bottom = False
instance HasTop Bool where top = True
instance Fixable Bool where
  isBottom = not
  isTop = id
  join = (||)
  meet = (&&)
  eq = (==)
  lte = (<=)


-- join is the maximum of integer values, as in this is the lattice of maximum, not the additive one.
instance Fixable Int where
  join = max
  meet = min
  lte = (<=)
  eq = (==)

-- bottom is zero and the lub is the maximum of integer values, as in this is the lattice of maximum, not the additive one.
instance HasBottom Word where bottom = 0
instance Fixable Word where
  isBottom = (==) 0
  join = max
  meet = min
  lte = (<=)
  eq = (==)


instance (HasBottom a, HasBottom b) => HasBottom (a, b) where bottom = (bottom, bottom)
instance (HasTop a, HasTop b) => HasTop (a, b) where top = (top, top)
instance (Fixable a, Fixable b) => Fixable (a, b) where
  isBottom (a, b) = isBottom a && isBottom b
  isTop (a, b) = isTop a && isTop b
  join (x, y) (x', y') = (join x x', join y y')
  meet (x, y) (x', y') = (meet x x', meet y y')
  lte (x, y) (x', y') = lte x x' && lte y y'
  eq (x, y) (x', y') = eq x x' && eq y y'


-- the maybe instance creates a new bottom of nothing. note that (Just bottom) is a distinct point.
instance HasBottom (Maybe a) where bottom = Nothing
instance HasTop a => HasTop (Maybe a) where top = Just top
instance Fixable a => Fixable (Maybe a) where
  isBottom = isNothing
  isTop = maybe False isTop
  join Nothing y = y
  join x Nothing = x
  join (Just x) (Just y) = Just (join x y)
  meet Nothing _ = Nothing
  meet _ Nothing = Nothing
  meet (Just x) (Just y) = Just (meet x y)
  lte Nothing _ = True
  lte _ Nothing = False
  lte (Just x) (Just y) = x `lte` y


-- the topped instance creates a new top of everything.
-- this is the opposite of the 'Maybe' instance
data Topped a = Only a | Top deriving (Eq, Ord, Show)


-- the maybe instance creates a new bottom of nothing. note that (Just bottom) is a distinct point.
instance HasBottom a => HasBottom (Topped a) where bottom = Only bottom
instance HasTop (Topped a) where top = Top
instance Fixable a => Fixable (Topped a) where
  isBottom (Only x) = isBottom x
  isBottom Top = False
  isTop Top = True
  isTop _ = False
  meet Top y = y
  meet x Top = x
  meet (Only x) (Only y) = Only (meet x y)
  join (Only x) (Only y) = Only (join x y)
  join _ _ = Top
  eq Top Top = True
  eq (Only x) (Only y) = eq x y
  eq _ _ = False
  lte _ Top = True
  lte Top _ = False
  lte (Only x) (Only y) = x `lte` y




-- vim: ts=8 sw=2 expandtab :

