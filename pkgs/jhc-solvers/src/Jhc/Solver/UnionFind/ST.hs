module Jhc.Solver.UnionFind.ST (
    Element,
    fromElement,
    --
    new, new_,
    find,
    getElements, getUnique,
    getWeight, putWeight, updateWeight,
    union, union_
    ) where

import Control.Monad.Reader
import Control.Monad.ST
import Data.STRef

import Control.Monad.Fresh.Flat (FreshST, fresh, liftST)

-- ---------------------------------------------------------------------------
data Link s w a = Weight !Int !w ![Element s w a]
                | Next !(Element s w a)
data Element s w a
  = Element { fromElement::a, elmKey:: !Int, elmLink:: !(STRef s (Link s w a)) }
instance Eq (Element s w a) where
  Element _ x _ == Element _ y _ = x == y
instance Ord (Element s w a) where
  Element _ x _ `compare` Element _ y _ = x `compare` y
instance Show a => Show (Element s w a) where
    show = show . fromElement


-- ---------------------------------------------------------------------------

new :: w -> a -> FreshST s (Element s w a)
new w x = fresh >>= liftST . \n -> do
    r <- newSTRef (Weight 1 w [])
    let ne = Element x n r
    writeSTRef r (Weight 1 w [ne])
    return ne

new_ ::  a -> FreshST s (Element s () a)
new_ = new ()

find' :: Element s w a -> ST s (Element s w a)
find' x@(Element _ _ r) = readSTRef r >>= \case
    Weight {} -> return x
    Next next -> do
      y <- find' next
      when (next /= y) $ writeSTRef r (Next y)
      return y


find ::  Element s w a -> FreshST s (Element s w a)
find = liftST . find'


getWeight ::  Element s w a -> FreshST s w
getWeight x = liftST $ do
    Weight _ w _ <- find' x >>= readSTRef . elmLink
    return w


-- retrieve list of unified elements
getElements :: Element s w a -> FreshST s [Element s w a]
getElements x = liftST $ do
    Weight _ _ es <- find' x >>= readSTRef . elmLink
    return es


getUnique ::  Element s w a -> FreshST s Int
getUnique x = elmKey <$> find x


-- update w returning the old value
updateWeight ::  (w -> w) -> Element s w a -> FreshST s w
updateWeight f x = liftST $ do
    r <- elmLink <$> find' x
    Weight _ w _ <- readSTRef r
    modifySTRef r (\ (Weight s w' es) -> Weight s (f w') es)
    return w


-- puts a new w, returning old value
putWeight ::  Element s w a -> w -> FreshST s w
putWeight e w = updateWeight (const w) e


union ::  (w -> w -> w) -> Element s w a -> Element s w a -> FreshST s ()
union comb e1 e2 = liftST $ do
    e1'@(Element _ _ r1) <- find' e1
    e2'@(Element _ _ r2) <- find' e2
    when (r1 /= r2) $ do
        Weight w1 x1 es1 <- readSTRef r1
        Weight w2 x2 es2 <- readSTRef r2
        if w1 <= w2 then do
            writeSTRef r1 (Next e2')
            writeSTRef r2 $! Weight (w1 + w2) (comb x1 x2) (es1 ++ es2)
          else do
            writeSTRef r1 $! Weight (w1 + w2) (comb x1 x2) (es1 ++ es2)
            writeSTRef r2 (Next e1')

union_ ::   Element s () a -> Element s () a -> FreshST s ()
union_ = union (\_ _ -> ())

-- vim: ts=8 sw=2 expandtab :

