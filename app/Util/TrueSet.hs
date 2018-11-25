module Util.TrueSet(
    TrueSet,
    fromList,
    member,
    empty,
    full,
    singleton,
    insert,
    delete,
    unions,
    union,
    intersection,
    intersects,
    difference,
    (\\)
    ) where

import qualified Data.Set as Set

-- infixl 9 \\


data TrueSet a = TrueSet (Set.Set a) Bool

fromList :: Ord a => [a] -> TrueSet a
fromList xs = TrueSet (Set.fromList xs) False

member :: Ord a => a -> TrueSet a -> Bool
member x (TrueSet s inv) = inv `xor` Set.member x s
  where
    False `xor` y = y
    True `xor` y = not y

invert :: TrueSet a -> TrueSet a
invert (TrueSet x y) = TrueSet x (not y)

empty :: TrueSet a
empty = TrueSet Set.empty False

full :: TrueSet a
full = TrueSet Set.empty True


singleton x = TrueSet (Set.singleton x) False
insert x (TrueSet s False) = TrueSet (Set.insert x s) False
insert x (TrueSet s True) = TrueSet (Set.delete x s) True
delete x (TrueSet s False) = TrueSet (Set.delete x s) False
delete x (TrueSet s True) = TrueSet (Set.insert x s) True

unions xs = foldlStrict union empty xs
intersects xs = foldlStrict intersection full xs

foldlStrict f z xs
  = case xs of
      []     -> z
      (x:xx) -> let z' = f z x in seq z' (foldlStrict f z' xx)



(\\), difference, intersection :: Ord a => TrueSet a -> TrueSet a -> TrueSet a
difference x y = x `intersection` invert y
m1 \\ m2 = difference m1 m2
(TrueSet x True)  `intersection` (TrueSet y True) = TrueSet (x `Set.union` y) True
(TrueSet x False) `intersection` (TrueSet y False) = TrueSet (x `Set.intersection` y) False
(TrueSet x True)  `intersection` (TrueSet y False) = TrueSet (y Set.\\ x) False
(TrueSet x False) `intersection` (TrueSet y True) = TrueSet (x Set.\\ y) False
(TrueSet x True)  `union` (TrueSet y True) = TrueSet (x `Set.intersection` y) True
(TrueSet x False) `union` (TrueSet y False) = TrueSet (x `Set.union` y) False
(TrueSet x True)  `union` (TrueSet y False) = TrueSet (x Set.\\ y) True
(TrueSet x False) `union` (TrueSet y True) = TrueSet (y Set.\\ x) True


