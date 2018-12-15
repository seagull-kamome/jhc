module Stats(
  Stat, fromStat,
  printStat,
  prepend, tick, ticks,
  --
  StatT, MonadStat (..)
    ) where

import Control.Monad.State.Strict
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import Data.Interned (intern, unintern)
import Data.Interned.Text

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

-- --------------------------------------------------------------------------

newtype Stat = Stat { fromStat :: Map.Map InternedText (Maybe Int, Maybe Stat) }
  deriving(Eq, Ord, Semigroup, Monoid)
instance Pretty Stat where
  pretty (Stat m) =
    mconcat [ text (T.unpack $ unintern k)
        <+> maybe empty (\v' -> colon <+> int v') v <> hardline
        <> maybe empty (indent 4 . pretty) c | (k, (v, c)) <- Map.toList m ]

printStat :: String -> Stat -> IO ()
printStat greet stat = putDoc $ text greet <> hardline <> indent 4 (pretty stat)

prepend :: T.Text -> Stat -> Stat
prepend (intern -> k) m = Stat $ Map.singleton k (Nothing, Just m)

ticks :: Int -> T.Text -> Stat -> Stat
ticks !n (intern -> k) (Stat m) = Stat $ Map.alter f k m
  where f Nothing = Just (Just n, Nothing)
        f (Just (Nothing, y)) = Just (Just n, y)
        f (Just (Just i, y)) = let !n' = n + i in Just (Just n', y)
tick :: T.Text -> Stat -> Stat
tick = ticks 1


-- ---------------------------------------------------------------------------

newtype StatT m a = StatT (StateT Stat m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

class MonadStat m where
  mticks :: Int -> T.Text -> m ()
  mtick :: T.Text -> m ()
  mtick = mticks 1

instance Monad m => MonadStat (StatT m) where
  mticks n k = StatT $ modify (ticks n k)


-- vim: ts=8 sw=2 expandtab :

