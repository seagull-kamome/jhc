module Util.RWS (module Control.Monad.RWS.Strict) where
import Control.Monad.RWS.Strict

#if 0
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
-- modified from Control.Monad.RWS by John Meacham to be strict

module Util.RWS (
	RWS,
        runRWS,
	module Control.Monad.Reader,
	module Control.Monad.Writer,
	module Control.Monad.State,
  ) where


import Prelude

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

newtype RWS r w s a = RWS { runRWS' :: r -> s -> (# a, s, w #) }

runRWS :: RWS r w s a -> r -> s -> (a,s,w)
runRWS x r s = case runRWS' x r s of
    (# a, b, c #) -> (a,b,c)

instance Functor (RWS r w s) where
	fmap f m = RWS $ \r s -> case runRWS' m r s of
		(# a, s', w #) -> (# f a, s', w #)

instance Monoid w => Applicative (WRS r w s) where
  pure a = RWS $ \_ s -> (# a, s, mempty #)
  (RWS f) (<*>) (RWS x) = 

instance (Monoid w) => Monad (RWS r w s) where
	return a = RWS $ \_ s -> (# a, s, mempty #)
	m >>= k  = RWS $ \r s -> case runRWS' m r s of
		(# a, s',  w #) -> case runRWS' (k a) r s' of
                    (# b, s'', w' #) -> let !w'' = w `mappend` w'
                        in (# b, s'', w'' #)
        RWS x >> RWS y   = RWS $ \r s -> case x r s of
            (# _, s', w' #) -> case y r s' of
                (# a, s'', w'' #) -> (# a, s'', w' `mappend` w'' #)

instance (Monoid w) => MonadReader r (RWS r w s) where
	ask       = RWS $ \r s -> (# r, s, mempty #)
	local f m = RWS $ \r s -> let !r' = f r in runRWS' m r' s

instance (Monoid w) => MonadWriter w (RWS r w s) where
	tell   w = RWS $ \_ s -> (# (), s, w #)
	listen m = RWS $ \r s -> case runRWS' m r s of
            (# a, s', w #) -> (# (a, w), s', w #)
	pass   m = RWS $ \r s -> case runRWS' m r s of
		(# (a, f), s', w #) -> let !w' = f w in (# a, s', w' #)

instance (Monoid w) => MonadState s (RWS r w s) where
	get   = RWS $ \_ s -> (# s, s, mempty #)
	put !s = RWS $ \_ _ -> (# (), s, mempty #)
#endif

