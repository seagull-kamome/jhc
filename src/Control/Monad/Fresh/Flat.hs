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
module Control.Monad.Fresh.Flat (
  MonadFresh(..),
  FreshT, Fresh, FreshST,

  -- re-export Control.Monad.Fresh
  Fresh.runFreshT, Fresh.runFresh,
  Fresh.runFreshST', Fresh.runFreshST, Fresh.liftST
  ) where

import Control.Monad
import Control.Monad.Identity
import Data.Unique (hashUnique, newUnique)
import qualified Control.Monad.Fresh as Fresh

-- ---------------------------------------------------------------------------
-- Same as MonadFresh but not naming universe.

class Monad m => MonadFresh m where fresh :: m Int
instance MonadFresh IO where fresh = Fresh.fresh ()
instance Monad m => MonadFresh (Fresh.FreshT () m) where fresh = Fresh.fresh ()
instance MonadFresh (Fresh.FreshST () s) where fresh = Fresh.fresh ()

type FreshT = Fresh.FreshT ()
type Fresh = Fresh.FreshT () Identity
type FreshST = Fresh.FreshST ()

-- ---------------------------------------------------------------------------

-- vim: ts=8 sw=2 expandtab :

