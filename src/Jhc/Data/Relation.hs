module Util.Relation (
  Relation, fromRelation,
  fromList, toList,
  empty, singleton, prune,
  union, unions,
  difference, intersection,
  domain, range,
  restrictDomain, restrictDomainS, restrictDomainSet,
  restrictRange,
  mapDomain, mapRange,
  partitionDomain,
  toRelationList
  ) where

import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import GHC.Exts (IsList(..))

-- --------------------------------------------------------------------------

newtype Relation a b = Relation { fromRelation :: Map a (Set b) }
  deriving (Eq)

instance (Ord a, Ord b) => Semigroup (Relation a b) where
  (Relation r1) <> (Relation r2) = Relation $ Map.unionWith Set.union r1 r2
  {-# INLINE (<>) #-}
instance (Ord a, Ord b) => Monoid (Relation a b) where
  mempty = Relation mempty
  {-# INLINE mempty #-}
instance (Ord a, Ord b) => IsList (Relation a b) where
  type Item (Relation a b) = (a, b)
  fromList xs = Relation $ Map.fromListWith Set.union [ (x, Set.singleton y) | (x, y) <- xs]
  {-# INLINE fromList #-}
  toList (Relation r) = [ (x, y) | (x, ys) <- Map.toList r, y <- Set.toList ys]
  {-# INLINE toList #-}



-- --------------------------------------------------------------------------
-- Utilities


-- --------------------------------------------------------------------------
-- Construction

empty :: (Ord a, Ord b) => Relation a b
empty = mempty
{-# INLINE empty #-}


singleton :: (Ord a, Ord b) => (a, b) -> Relation a b
singleton (x, y) = Relation $ Map.singleton x (Set.singleton y)
{-# INLINE singleton #-}


prune :: (Ord a, Ord b) => Map a (Set b) -> Relation a b
prune r = Relation $ Map.mapMaybe f r
  where f s = if Set.null s then Nothing else Just s
{-# INLINE prune #-}


-- --------------------------------------------------------------------------
-- Union

union :: (Ord a, Ord b) => Relation a b ->Relation a b -> Relation a b
union x y = x <> y
{-# INLINE union #-}

unions :: (Ord a, Ord b) => [Relation a b] -> Relation a b
unions = foldl' union empty
{-# INLINE unions #-}


-- --------------------------------------------------------------------------
-- Difference

difference :: (Ord a, Ord b) => Relation a b -> Relation a b -> Relation a b
difference (Relation r1) (Relation r2) = Relation $ Map.differenceWith f r1 r2
  where f s1 s2 = let s = Set.difference s1 s2
                   in if Set.null s then Nothing else Just s
{-# INLINE difference #-}

-- --------------------------------------------------------------------------
-- Intersection

intersection :: (Ord a, Ord b) => Relation a b -> Relation a b -> Relation a b
intersection (Relation r1) (Relation r2) =
  prune $ Map.intersectionWith Set.intersection r1 r2
{-# INLINE intersection #-}


-- --------------------------------------------------------------------------

domain :: (Ord a, Ord b) => Relation a b -> Set a
domain = Map.keysSet . fromRelation
{-# INLINE domain #-}

range :: (Ord a, Ord b) => Relation a b -> Set b
range = Set.unions . Map.elems . fromRelation
{-# INLINE range #-}



restrictDomain :: (Ord a, Ord b) => (a -> Bool) -> Relation a b -> Relation a b
restrictDomain f = Relation . Map.filterWithKey (\k _ -> f k) . fromRelation
{-# INLINE restrictDomain #-}

restrictDomainS :: (Ord a, Ord b) => a -> Relation a b -> Relation a b
restrictDomainS x = Relation . maybe Map.empty (Map.singleton x) . Map.lookup x . fromRelation
{-# INLINE restrictDomainS #-}

restrictDomainSet :: (Ord a, Ord b) => Set a -> Relation a b -> Relation a b
restrictDomainSet s = Relation . Map.filterWithKey (\k _ -> Set.member k s) . fromRelation
{-# INLINE restrictDomainSet #-}


restrictRange :: (Ord a, Ord b) => (b -> Bool) -> Relation a b -> Relation a b
restrictRange f (Relation r) = Relation $ Map.mapMaybe g r
  where g s = let ss = Set.filter f s
               in if Set.null ss then Nothing else Just ss
{-# INLINE restrictRange #-}

mapDomain :: (Ord a, Ord b, Ord c) => (a -> c) -> Relation a b -> Relation c b
mapDomain f = Relation . Map.mapKeys f . fromRelation
{-# INLINE mapDomain #-}

mapRange :: (Ord a, Ord b, Ord c) => (b -> c) -> Relation a b -> Relation a c
mapRange f = Relation . Map.map (Set.map f) . fromRelation
{-# INLINE mapRange #-}

partitionDomain :: (Ord a, Ord b)
                => (a -> Bool) -> Relation a b -> (Relation a b, Relation a b)
partitionDomain f (Relation r) = (Relation x, Relation y)
  where (x, y) = Map.partitionWithKey (\k _ -> f k) r
{-# INLINE partitionDomain #-}


toRelationList :: (Ord a, Ord b) => Relation a b -> [(a, [b])]
toRelationList = Map.toList . Map.map Set.toList . fromRelation
{-# INLINE toRelationList #-}


-- vim: ts=8 sw=2 expandtab :

