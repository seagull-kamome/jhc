{-
Copyright Hattori, Hiroki (c) 2018

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Hattori, Hiroki nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
module Control.Monad.Fresh (
  MonadFresh(..),
  FreshT, Fresh
  ) where

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Identity
import Data.Unique

-- ---------------------------------------------------------------------------
-- ! FreshMonad produce unique value for each fresh call.

class Monad m => MonadFresh m where fresh :: m Int

instance MonadFresh IO where fresh = hashUnique <$> newUnique

-- ---------------------------------------------------------------------------

newtype FreshT m r = FreshT (StateT Int m r)
  deriving (Functor, Applicative, Monad)
type Fresh r = FreshT Identity r

instance MonadTrans FreshT where lift = FreshT . lift
instance MonadIO m => MonadIO (FreshT m) where liftIO = FreshT . liftIO
instance Monad m => MonadFresh (FreshT m) where
  fresh = FreshT $ do { i <- get; put $! (i + 1); return i }

runFreshT :: Monad m => FreshT m r -> Int -> m (r, Int)
runFreshT (FreshT st) = runStateT st

evalFreshT :: Monad m => FreshT m r -> Int -> m r
evalFreshT (FreshT st) = evalStateT st

execFreshT :: Monad m => FreshT m r -> Int -> m Int
execFreshT (FreshT st) = execStateT st


runFresh :: Fresh r -> Int -> (r, Int)
runFresh (FreshT st) = runIdentity . runStateT st

evalFresh :: Fresh r -> Int -> r
evalFresh (FreshT st) = runIdentity . evalStateT st

execFresh :: Fresh r -> Int -> Int
execFresh (FreshT st) = runIdentity . execStateT st

-- ---------------------------------------------------------------------------

-- vim: ts=8 sw=2 expandtab :

