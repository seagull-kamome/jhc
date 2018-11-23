{-# LANGUAGE CPP,NoBangPatterns,MagicHash #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IntBag
-- Copyright   :  (c) Daan Leijen 2002
-- Copyright   :  (c) John Meacham 2007
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- An efficient implementation of maps from integer keys to integers.
--
-- modified from Data.IntMap
--

module Util.IntBag  (
            -- * Map type
              IntBag, Key          -- instance Eq,Show

            -- * Operators
            , (!)

            -- * Query
            , null , size

            -- * Construction
            , empty , singleton , msingleton

            -- ** Insertion
            , insert

            -- ** Delete\/Update
            , delete

            -- ** Union
            , union

            -- ** Fold
            , fold , foldWithKey

            -- * Conversion
            , assocs

            -- ** Lists
            , toList , fromList

            -- ** Ordered lists
            , toAscList
            ) where


import Prelude hiding (lookup,map,filter,foldr,foldl,null)
import Data.Bits
import Data.Monoid (Monoid(..))
import GHC.Word
import GHC.Exts (  Int(..), shiftRL# )

--infixl 9 \\{-This comment teaches CPP correct behaviour -}

-- A "Nat" is a natural machine word (an unsigned Int)
type Nat = Word

natFromInt :: Key -> Nat
natFromInt i = fromIntegral i

intFromNat :: Nat -> Key
intFromNat w = fromIntegral w

shiftRL :: Nat -> Key -> Nat
{--------------------------------------------------------------------
  GHC: use unboxing to get @shiftRL@ inlined.
--------------------------------------------------------------------}
shiftRL (W# x) (I# i)
  = W# (shiftRL# x i)

{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}

-- | /O(min(n,W))/. Find the value at a key.
-- Calls 'error' when the element can not be found.

(!) :: IntBag-> Key -> Int
m ! k    = find' k m

-- | /O(n+m)/. See 'difference'.
--(\\) :: IntBag -> IntBag -> IntBag
--m1 \\ m2 = difference m1 m2

{--------------------------------------------------------------------
  Types
--------------------------------------------------------------------}
-- | A map of integers to values @a@.
data IntBag = Nil
              | Tip {-# UNPACK #-} !Key {-# UNPACK #-} !Int
              | Bin {-# UNPACK #-} !Prefix {-# UNPACK #-} !Mask !IntBag !IntBag

type Prefix = Int
type Mask   = Int
type Key    = Int

instance Semigroup IntBag where (<>) = union
instance Monoid IntBag where
    mempty  = empty
    -- mappend = union
    mconcat = unions



{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}
-- | /O(1)/. Is the map empty?
null :: IntBag -> Bool
null Nil   = True
null other = False

-- | /O(n)/. Number of elements in the map.
size :: IntBag -> Int
size t
  = case t of
      Bin p m l r -> size l + size r
      Tip k x -> x
      Nil     -> 0

lookupN :: Nat -> IntBag -> Int
lookupN k t
  = case t of
      Bin p m l r
        | zeroN k (natFromInt m) -> lookupN k l
        | otherwise              -> lookupN k r
      Tip kx x
        | (k == natFromInt kx)  -> x
        | otherwise             -> 0
      Nil -> 0

find' :: Key -> IntBag -> Int
find' k m  = lookupN (natFromInt k) m


{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
-- | /O(1)/. The empty map.
empty :: IntBag
empty = Nil

-- | /O(1)/. A map of one element.
singleton :: Key -> IntBag
singleton k = Tip k 1

msingleton :: Key -> Int -> IntBag
msingleton k x | x > 0 = Tip k x
               | otherwise = Nil

{--------------------------------------------------------------------
  Insert
--------------------------------------------------------------------}
-- | /O(min(n,W))/. Insert a new key\/value pair in the map.
-- If the key is already present in the map, the associated value is
-- added to the supplied value, i.e. 'insert' is equivalent to
-- @'insertWith' 'const'@.
insert :: Key -> Int -> IntBag -> IntBag
insert k x t | k `seq` x < 0 = delete k (negate x) t
insert _ 0 t = t
insert k x t = f t where
    f t = case t of
      Bin p m l r
        | nomatch k p m -> join k (Tip k x) p t
        | zero k m      -> Bin p m (f l) r
        | otherwise     -> Bin p m l (f r)
      Tip ky y
        | k==ky         -> Tip k (x + y)
        | otherwise     -> join k (Tip k x) ky t
      Nil -> Tip k x



{--------------------------------------------------------------------
  Deletion
  [delete] is the inlined version of [deleteWith (\k x -> Nothing)]
--------------------------------------------------------------------}
-- | /O(min(n,W))/. Delete a key and its value from the map. When the key is not
-- a member of the map, the original map is returned.
delete :: Key -> Int -> IntBag -> IntBag
delete k 0 t | k `seq` True = t
delete k x t | x < 0 = insert k (negate x) t
delete k x t = f t where
    f t = case t of
      Bin p m l r
        | nomatch k p m -> t
        | zero k m      -> bin p m (f l) r
        | otherwise     -> bin p m l (f r)
      Tip ky y
        | k==ky         -> if y < x then Nil else Tip ky (y - x)
        | otherwise     -> t
      Nil -> Nil

{--------------------------------------------------------------------
  Union
--------------------------------------------------------------------}
-- | The union of a list of maps.
unions :: [IntBag] -> IntBag
unions xs
  = foldlStrict union empty xs

-- | The union of a list of maps, with a combining operation

-- | /O(n+m)/. The (left-biased) union of two maps.
-- It prefers the first map when duplicate keys are encountered,
-- i.e. (@'union' == 'unionWith' 'const'@).
union :: IntBag -> IntBag -> IntBag
union t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2  = union1
  | shorter m2 m1  = union2
  | p1 == p2       = Bin p1 m1 (union l1 l2) (union r1 r2)
  | otherwise      = join p1 t1 p2 t2
  where
    union1  | nomatch p2 p1 m1  = join p1 t1 p2 t2
            | zero p2 m1        = Bin p1 m1 (union l1 t2) r1
            | otherwise         = Bin p1 m1 l1 (union r1 t2)

    union2  | nomatch p1 p2 m2  = join p1 t1 p2 t2
            | zero p1 m2        = Bin p2 m2 (union t1 l2) r2
            | otherwise         = Bin p2 m2 l2 (union t1 r2)

union (Tip k x) t = insert k x t
union t (Tip k x) = insert k x t
union Nil t       = t
union t Nil       = t

{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}
-- | /O(n)/. Fold the values in the map, such that
-- @'fold' f z == 'Prelude.foldr' f z . 'elems'@.
-- For example,
--
-- > elems map = fold (:) [] map
--
fold :: (Int -> b -> b) -> b -> IntBag -> b
fold f z t
  = foldWithKey (\k x y -> f x y) z t

-- | /O(n)/. Fold the keys and values in the map, such that
-- @'foldWithKey' f z == 'Prelude.foldr' ('uncurry' f) z . 'toAscList'@.
-- For example,
--
-- > keys map = foldWithKey (\k x ks -> k:ks) [] map
--
foldWithKey :: (Key -> Int -> b -> b) -> b -> IntBag -> b
foldWithKey f z t
  = foldr f z t

foldr :: (Key -> Int -> b -> b) -> b -> IntBag -> b
foldr f z t
  = case t of
      Bin 0 m l r | m < 0 -> foldr' f (foldr' f z l) r  -- put negative numbers before.
      Bin _ _ _ _ -> foldr' f z t
      Tip k x     -> f k x z
      Nil         -> z

foldr' :: (Key -> Int -> b -> b) -> b -> IntBag -> b
foldr' f z t
  = case t of
      Bin p m l r -> foldr' f (foldr' f z r) l
      Tip k x     -> f k x z
      Nil         -> z



{--------------------------------------------------------------------
  List variations
--------------------------------------------------------------------}
-- | /O(n)/. Return all key\/value pairs in the map in ascending key order.
assocs :: IntBag -> [(Key,Int)]
assocs m = toList m


{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}
-- | /O(n)/. Convert the map to a list of key\/value pairs.
toList :: IntBag -> [(Key,Int)]
toList t
  = foldWithKey (\k x xs -> (k,x):xs) [] t

-- | /O(n)/. Convert the map to a list of key\/value pairs where the
-- keys are in ascending order.
toAscList :: IntBag -> [(Key,Int)]
toAscList t
  = -- NOTE: the following algorithm only works for big-endian trees
    let (pos,neg) = span (\(k,x) -> k >=0) (foldr (\k x xs -> (k,x):xs) [] t) in neg ++ pos

-- | /O(n*min(n,W))/. Create a map from a list of key\/value pairs.
fromList :: [(Key,Int)] -> IntBag
fromList xs
  = foldlStrict ins empty xs
  where
    ins t (k,x)  = insert k x t

{--------------------------------------------------------------------
  Eq
--------------------------------------------------------------------}
instance Eq IntBag where
  t1 == t2  = equal t1 t2
  t1 /= t2  = nequal t1 t2

equal :: IntBag -> IntBag -> Bool
equal (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  = (m1 == m2) && (p1 == p2) && (equal l1 l2) && (equal r1 r2)
equal (Tip kx x) (Tip ky y)
  = (kx == ky) && (x==y)
equal Nil Nil = True
equal t1 t2   = False

nequal :: IntBag -> IntBag -> Bool
nequal (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  = (m1 /= m2) || (p1 /= p2) || (nequal l1 l2) || (nequal r1 r2)
nequal (Tip kx x) (Tip ky y)
  = (kx /= ky) || (x/=y)
nequal Nil Nil = False
nequal t1 t2   = True

{--------------------------------------------------------------------
  Ord
--------------------------------------------------------------------}

instance Ord IntBag where
    compare m1 m2 = compare (toList m1) (toList m2)


{--------------------------------------------------------------------
  Show
--------------------------------------------------------------------}

instance Show IntBag where
  showsPrec d m   = showParen (d > 10) $
    showString "fromList " . shows (toList m)
{--------------------------------------------------------------------
  Join
--------------------------------------------------------------------}
join :: Prefix -> IntBag -> Prefix -> IntBag -> IntBag
join p1 t1 p2 t2
  | zero p1 m = Bin p m t1 t2
  | otherwise = Bin p m t2 t1
  where
    m = branchMask p1 p2
    p = mask p1 m

{--------------------------------------------------------------------
  @bin@ assures that we never have empty trees within a tree.
--------------------------------------------------------------------}
bin :: Prefix -> Mask -> IntBag -> IntBag -> IntBag
bin p m l Nil = l
bin p m Nil r = r
bin p m l r   = Bin p m l r


{--------------------------------------------------------------------
  Endian independent bit twiddling
--------------------------------------------------------------------}
zero :: Key -> Mask -> Bool
zero i m
  = (natFromInt i) .&. (natFromInt m) == 0

nomatch,_match :: Key -> Prefix -> Mask -> Bool
nomatch i p m
  = (mask i m) /= p

_match i p m
  = (mask i m) == p

mask :: Key -> Mask -> Prefix
mask i m
  = maskW (natFromInt i) (natFromInt m)


zeroN :: Nat -> Nat -> Bool
zeroN i m = (i .&. m) == 0

{--------------------------------------------------------------------
  Big endian operations
--------------------------------------------------------------------}
maskW :: Nat -> Nat -> Prefix
maskW i m
  = intFromNat (i .&. (complement (m-1) `xor` m))

shorter :: Mask -> Mask -> Bool
shorter m1 m2
  = (natFromInt m1) > (natFromInt m2)

branchMask :: Prefix -> Prefix -> Mask
branchMask p1 p2
  = intFromNat (highestBitMask (natFromInt p1 `xor` natFromInt p2))

{----------------------------------------------------------------------
  Finding the highest bit (mask) in a word [x] can be done efficiently in
  three ways:
  * convert to a floating point value and the mantissa tells us the
    [log2(x)] that corresponds with the highest bit position. The mantissa
    is retrieved either via the standard C function [frexp] or by some bit
    twiddling on IEEE compatible numbers (float). Note that one needs to
    use at least [double] precision for an accurate mantissa of 32 bit
    numbers.
  * use bit twiddling, a logarithmic sequence of bitwise or's and shifts (bit).
  * use processor specific assembler instruction (asm).

  The most portable way would be [bit], but is it efficient enough?
  I have measured the cycle counts of the different methods on an AMD
  Athlon-XP 1800 (~ Pentium III 1.8Ghz) using the RDTSC instruction:

  highestBitMask: method  cycles
                  --------------
                   frexp   200
                   float    33
                   bit      11
                   asm      12

  highestBit:     method  cycles
                  --------------
                   frexp   195
                   float    33
                   bit      11
                   asm      11

  Wow, the bit twiddling is on today's RISC like machines even faster
  than a single CISC instruction (BSR)!
----------------------------------------------------------------------}

{----------------------------------------------------------------------
  [highestBitMask] returns a word where only the highest bit is set.
  It is found by first setting all bits in lower positions than the
  highest bit and than taking an exclusive or with the original value.
  Allthough the function may look expensive, GHC compiles this into
  excellent C code that subsequently compiled into highly efficient
  machine code. The algorithm is derived from Jorg Arndt's FXT library.
----------------------------------------------------------------------}
highestBitMask :: Nat -> Nat
highestBitMask x
  = case (x .|. shiftRL x 1) of
     x -> case (x .|. shiftRL x 2) of
      x -> case (x .|. shiftRL x 4) of
       x -> case (x .|. shiftRL x 8) of
        x -> case (x .|. shiftRL x 16) of
         x -> case (x .|. shiftRL x 32) of   -- for 64 bit platforms
          x -> (x `xor` (shiftRL x 1))


{--------------------------------------------------------------------
  Utilities
--------------------------------------------------------------------}
foldlStrict f z xs
  = case xs of
      []     -> z
      (x:xx) -> let z' = f z x in seq z' (foldlStrict f z' xx)


