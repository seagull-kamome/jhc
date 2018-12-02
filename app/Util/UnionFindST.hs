{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Util.UnionFindST(
    Element,
    fromElement,
    --
    UF(), liftST, runUF,
    new, new_,
    find,
    getElements, getUnique,
    getW, putW, updateW,
    union, union_
    ) where

import Control.Monad.Reader
import Control.Monad.ST
import Data.STRef

-- ---------------------------------------------------------------------------

data Element s w a = Element a !Int {-# UNPACK #-} !(STRef s (Link s w a))
instance Eq (Element s w a) where
  Element _ x _ == Element _ y _ = x == y
instance Ord (Element s w a) where
  Element _ x _ `compare` Element _ y _ = x `compare` y
instance Show a => Show (Element s w a) where
    show = show . fromElement

fromElement :: Element s w a -> a
fromElement (Element a _ _) = a


-- ---------------------------------------------------------------------------

data Link s w a = Weight !Int !w ![Element s w a]
                | Next !(Element s w a)


-- ---------------------------------------------------------------------------

newtype UF s a = UF { unUF :: ReaderT (STRef s Int) (ST s) a }
    deriving (Functor, Applicative, Monad)

runUF :: forall a . (forall s . UF s a)  -> a
runUF st = runST $ newSTRef 0 >>= runReaderT (unUF st)

newUnique' :: STRef s Int -> ST s Int
newUnique' ref = do
  u <- (+1) <$> readSTRef ref
  writeSTRef ref u
  return u



new :: w -> a -> UF s (Element s w a)
new w x = UF $ ask >>= \ref -> lift $ do
    r <- newSTRef (Weight 1 w [])
    ne <- Element x <$> newUnique' ref <*> pure r
    writeSTRef r (Weight 1 w [ne])
    return ne

new_ ::  a -> UF s (Element s () a)
new_ = new ()

liftST :: ST s a -> UF s a
liftST = UF . lift



find' :: Element s w a -> ST s (Element s w a)
find' x@(Element _ _ r) = readSTRef r >>= \case
    Weight {} -> return x
    Next next -> do
      y <- find' next
      when (next /= y) $ writeSTRef r (Next y)
      return y


find ::  Element s w a -> UF s (Element s w a)
find = UF . lift . find'


getW ::  Element s w a -> UF s w
getW x = UF $ lift $ do
    Element _ _ r <- find' x
    Weight _ w _ <- readSTRef  r
    return w


-- retrieve list of unified elements
getElements :: Element s w a -> UF s [Element s w a]
getElements x = UF $ lift $ do
    Element _ _ r <- find' x
    Weight _ _ es <- readSTRef  r
    return es


getUnique ::  Element s w a -> UF s Int
getUnique x = do
    Element _ u _ <- find x
    return u


-- update w returning the old value
updateW ::  (w -> w) -> Element s w a -> UF s w
updateW f x = UF $ lift $ do
    Element _ _ r <- find' x
    Weight _ w _ <- readSTRef  r
    modifySTRef r (\ (Weight s w' es) -> Weight s (f w') es)
    return w


-- puts a new w, returning old value
putW ::  Element s w a -> w -> UF s w
putW e w = updateW (const w) e


union ::  (w -> w -> w) -> Element s w a -> Element s w a -> UF s ()
union comb e1 e2 = UF $ lift $ do
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

union_ ::   Element s () a -> Element s () a -> UF s ()
union_ = union (\_ _ -> ())

-- vim: ts=8 sw=2 expandtab :

