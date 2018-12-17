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
  FreshT, Fresh,
  runFreshT, runFresh,
  ) where

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Identity
import Data.Unique (hashUnique, newUnique)
import Data.Ix

import qualified Data.Array.Unboxed as AR


-- ---------------------------------------------------------------------------
-- ! FreshMonad produce unique value on its space, for each fresh call.

class Monad m => MonadFresh univ m | m -> univ where
  fresh :: univ -> m Int

instance MonadFresh () IO where
  fresh _ = hashUnique <$> newUnique

-- ---------------------------------------------------------------------------

newtype FreshT univ m r = FreshT (StateT (AR.UArray univ Int) m r)
  deriving (Functor, Applicative, Monad)
type Fresh univ = FreshT univ Identity

instance MonadTrans (FreshT univ) where lift = FreshT . lift
instance MonadIO m => MonadIO (FreshT univ m) where liftIO = FreshT . liftIO

instance (Ix univ, Monad m) => MonadFresh univ (FreshT univ m) where
  fresh u = FreshT $ do
    xs <- get
    let n = xs AR.! u
    put $ xs AR.// [(u, n + 1)]
    return n


runFreshT :: (Enum univ, Bounded univ, Ix univ, Monad m) => FreshT univ m r -> Int -> m r
runFreshT (FreshT st) n = evalStateT st $
  AR.array (minBound, maxBound) [ (x, n) | x <- [minBound .. maxBound] ]

runFresh :: (Enum univ, Bounded univ, Ix univ) => Fresh univ r -> Int -> r
runFresh x n = runIdentity $ runFreshT x n

-- ---------------------------------------------------------------------------

-- vim: ts=8 sw=2 expandtab :

