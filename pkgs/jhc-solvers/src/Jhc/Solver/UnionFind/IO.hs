{-# LANGUAGE LambdaCase #-}
module Jhc.Solver.UnionFind.IO (
    Element,
    fromElement,
    --
    new, new_,
    find,
    getWeight, putWeight, updateWeight,
    union, union_
    ) where

import Control.Monad.Trans
import Data.IORef
import Control.Monad (when)

import Control.Monad.Fresh.Flat (MonadFresh, fresh)

-- ---------------------------------------------------------------------------

data Link w a = Weight !Int w | Next (Element w a)
data Element w a
  = Element { fromElement::a, elmKey:: !Int, elmLink:: !(IORef (Link w a)) }
instance Eq (Element w a) where
  Element _ x _ == Element _ y _ = x == y
instance Ord (Element w a) where
  Element _ x _ `compare` Element _ y _ = x `compare` y
instance Show a => Show (Element w a) where
  show (Element x _ _) = show x


-- ---------------------------------------------------------------------------

new :: (MonadIO m, MonadFresh m)  => w -> a -> m (Element w a)
new w x = liftIO $  Element x <$> fresh <*> (liftIO $ newIORef (Weight 1 w))

new_ :: (MonadIO m, MonadFresh m) => a -> m (Element () a)
new_ = new ()



find :: MonadIO m => Element w a -> m (Element w a)
find x@(Element _ _ r) = liftIO $ readIORef r >>= \case
    Weight _ _ -> return x
    Next next -> do
      y <- find next
      when (next /= y) $ writeIORef r (Next y)
      return y



getWeight :: MonadIO m => Element w a -> m w
getWeight x = liftIO $ do
  Weight _ w <- find x >>= readIORef . elmLink
  return w



updateWeight :: MonadIO m => (w -> w) -> Element w a -> m ()
updateWeight f x = liftIO $
  find x >>= flip modifyIORef (\ (Weight s w) -> Weight s (f w)) . elmLink



putWeight :: MonadIO m => Element w a -> w -> m ()
putWeight e w = updateWeight (const w) e



union :: MonadIO m => (w -> w -> w) -> Element w a -> Element w a -> m ()
union comb e1 e2 = liftIO $ do
  e1'@(Element _ _ r1) <- find e1
  e2'@(Element _ _ r2) <- find e2
  when (r1 /= r2) $ do
    Weight w1 x1 <- readIORef r1
    Weight w2 x2 <- readIORef r2
    if w1 <= w2 then do
        writeIORef r1 (Next e2')
        writeIORef r2 $! Weight (w1 + w2) (comb x1 x2)
      else do
        writeIORef r1 $! Weight (w1 + w2) (comb x1 x2)
        writeIORef r2 (Next e1')



union_ :: MonadIO m =>  Element () a -> Element () a -> m ()
union_ = union (\_ _ -> ())


