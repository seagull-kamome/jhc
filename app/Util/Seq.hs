module Util.Seq
    (Seq()
    ,Util.Seq.concat
    ,appendToList
    ,cons
    ,fromList
    ,singleton
    ,snoc
    ,toList) where

import Control.Applicative
import Control.Monad
import Data.Foldable (Foldable(..))
import Data.Monoid (Monoid(..))
import Data.Traversable(Traversable(..))

newtype Seq a = Seq ([a] -> [a])

singleton :: a -> Seq a
singleton x = Seq (\ts -> x:ts)

cons :: a -> Seq a -> Seq a
cons x (Seq f) = Seq (\ts -> x:f ts)

snoc :: Seq a -> a ->  Seq a
snoc (Seq f) x = Seq (\ts -> f (x:ts))

appendToList :: Seq a -> [a] -> [a]
appendToList (Seq f) xs = f xs

fromList :: [a] -> Seq a
fromList xs = Seq (\ts -> xs++ts)

concat :: Seq (Seq a) -> Seq a
concat (Seq f) = (Prelude.foldr mappend mempty (f []))

instance Functor Util.Seq.Seq where
    --fmap f xs = Seq.fromList (map f (Seq.toList xs))
    fmap f (Seq xs) = Seq (\ts -> map f (xs []) ++ ts )

instance Monad Util.Seq.Seq where
    --a >>= b  = mconcat ( fmap b (Seq.toList a))
    a >>= b  = Util.Seq.concat (fmap b a)
    return = singleton
    fail _ = mempty

instance Applicative Seq where
    pure = return
    (<*>) = ap
instance Alternative Seq where
    empty = mempty
    (<|>)  = mappend


instance Traversable Seq where
    traverse f (Seq g) = fmap fromList (traverse f (g []))

instance Foldable Util.Seq.Seq where
    foldMap f s = mconcat (map f (toList s))
    toList (Seq f) = f []

instance MonadPlus Util.Seq.Seq where
    -- mplus = mappend
    -- mzero = mempty

instance Semigroup (Seq a) where Seq f <> Seq g = Seq (f . g)
instance Monoid (Seq a) where mempty = Seq id
    --Seq f `mappend` Seq g = Seq (\xs -> f (g xs))

instance Show a => Show (Seq a) where
    showsPrec n s = showsPrec n (toList s)
