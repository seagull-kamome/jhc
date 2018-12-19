-- find fixpoint of constraint problem

{- 2009.01.05: Lemmih

This may be obvious to a lot of people but it certainly wasn't obvious to me.

The following module help you solve problems that involve iterating over
a piece of data until some steady-state (aka. a fixpoint) is found.

One example problem would be dead-code elimination. To remove all dead
functions and function arguments, we have to mark everything that
could possibly be alive (we necessarily have to be conservative).
This is done in two steps:
1) Walk through the code and make a note of all the dependencies
   (eg. function 'x' uses function 'y' and function 'z'). The dependencies
   are then handed over to the fixpoint solver.
2) The fixpoint solver iterate over all the data and use the dependencies
   to propagate the usage information. That is, if 'x' is used then 'y' and 'z'
   are as well. The next iteration will deal with the dependencies of 'y' and 'z'.

Once there's no more usage information to propagate, we know we've found our fixpoint.
There are several other problems that require fixpoint iteration. Perhaps the most
distinguished is the heap points-to analysis we use to eliminate eval/apply calls.

-}
module Jhc.Solver.Fixer.Fixer (
    Value(IOValue, ConstValue), Rule(), Fixer(),
    addRule,
    ioToRule,
    conditionalRule,
    dynamicRule,
    findFixpoint,
    calcFixpoint,
    isSuperSetOf,
    modifiedSuperSetOf,
    newFixer,
    newValue,
    readValue,
    readRawValue,
    ) where

import Control.Monad (unless)
import Control.Monad.Trans
import Data.Maybe (fromMaybe)
import Data.IORef
import Data.Typeable
import Data.Unique
import qualified Data.Set as Set
import GHC.Exts (IsString(fromString))

-- import Control.Monad.Fresh.Flat (MonadFresh(..))
import qualified Jhc.Logging as LOG
import Jhc.Solver.Fixer.Fixable (Fixable(..), HasBottom(bottom))


-- ---------------------------------------------------------------------------
-- Types

data RvValue a = RvValue {
    ident :: !Unique,
    action :: {-# UNPACK #-} !(IORef [a -> IO ()]),
    pending :: {-# UNPACK #-} !(IORef a),
    current :: {-# UNPACK #-} !(IORef a),
    fixer :: Fixer
  }


data MkFixable = forall a . (HasBottom a, Fixable a) => MkFixable (RvValue a)
instance Eq MkFixable where
  MkFixable a == MkFixable b = ident a == ident b
  MkFixable a /= MkFixable b = ident a /= ident b
instance Ord MkFixable where
  MkFixable a `compare` MkFixable b = ident a `compare` ident b
  MkFixable a >= MkFixable b = ident a >= ident b
  MkFixable a <= MkFixable b = ident a <= ident b
  MkFixable a > MkFixable b = ident a > ident b
  MkFixable a < MkFixable b = ident a < ident b



data Fixer  = Fixer {
    vars :: {-# UNPACK #-} !(IORef [MkFixable]),
    todo :: {-# UNPACK #-} !(IORef (Set.Set MkFixable))
  }



data Value a = IOValue (IO (Value a)) | UnionValue (Value a) (Value a) | ConstValue a | IV (RvValue a)
  deriving(Typeable)
instance Fixable a => Semigroup (Value a) where
  a <> b = UnionValue a b
instance (Fixable a, HasBottom a) => Monoid (Value a) where
  mempty = ConstValue bottom
instance Fixable a => Show (Value a) where
  show (ConstValue a) = "<<" ++ showFixable a ++ ">>"
  show (UnionValue _ b) = "<<" ++ show b ++ ">>"
  show (IOValue _) = "<<IO>>"
  show (IV a) = "<<" ++ show (hashUnique $ ident a) ++  ">>"



-- ---------------------------------------------------------------------------

newtype Rule = Rule { unRule :: IO () } deriving(Typeable)
instance Semigroup Rule where
  (Rule x) <> (Rule y) = Rule $ x >> y
instance Monoid Rule where
  mempty = Rule (return ())
  mconcat rs = Rule $ sequence_ $ map unRule rs




-- ---------------------------------------------------------------------------
--

newFixer :: MonadIO m => m Fixer
newFixer = liftIO $ Fixer <$> newIORef [] <*> newIORef Set.empty


newValue :: (MonadIO m, HasBottom a, Fixable a) => Fixer -> a -> m (Value a)
newValue fixer@Fixer { vars = vars } v = liftIO $ do
    rv <- RvValue <$> newUnique <*> newIORef [] <*> newIORef bottom <*> newIORef bottom <*> pure fixer
    modifyIORef vars (MkFixable rv:)
    propagateValue v rv
    return $ IV rv


addAction :: Fixable a => Value a -> (a -> IO ())  -> IO ()
addAction (ConstValue n) act = act n
addAction (UnionValue a b) act = addAction a act >> addAction b act
addAction (IOValue v) act = v >>= (`addAction` act)
addAction (IV v) act = do
    modifyIORef (action v) (act:)
    c <- readIORef (current v)
    unless (isBottom c) (act c)



-- | add a rule to the current set
addRule :: MonadIO m => Rule -> m ()
addRule (Rule act) = liftIO act

-- | turn an IO action into a Rule
ioToRule :: IO () -> Rule
ioToRule act = Rule act



-- | the function must satisfy the rule that if a >= b then f(a) >= f(b)
modifiedSuperSetOf :: (Fixable a, HasBottom b, Fixable b) =>  Value b -> Value a -> (a -> b) -> Rule
modifiedSuperSetOf (IV rv) (ConstValue cv) r = Rule $ propagateValue (r cv) rv
modifiedSuperSetOf (IV rv) v2 r = Rule $ addAction v2 (\x -> propagateValue (r x) rv)
modifiedSuperSetOf (IOValue iov) v2 r = Rule $ iov >>= \v1 -> unRule $ modifiedSuperSetOf v1 v2 r
modifiedSuperSetOf (ConstValue vb) (ConstValue va)  f | f va `lte` vb =  Rule $ return ()
modifiedSuperSetOf ca@ConstValue {}  cb _ =  Rule $ fail ("Fixer.modifedSuperSetOf: You cannot modify a constant value:" ++ show(ca,cb))
modifiedSuperSetOf UnionValue {} _ _ =  Rule $ fail "Fixer: You cannot modify a union value"



isSuperSetOf :: (HasBottom a, Fixable a) => Value a -> Value a -> Rule
(IV rv) `isSuperSetOf` (ConstValue v2) = Rule $ propagateValue v2 rv
(IV rv) `isSuperSetOf` v2 = Rule $ addAction v2 (\x -> propagateValue x rv)
(IOValue iov) `isSuperSetOf` v2 = Rule $ iov >>= unRule . (`isSuperSetOf` v2)
ConstValue v1 `isSuperSetOf` ConstValue v2 | v2 `lte` v1 =  Rule $ return ()
ConstValue {} `isSuperSetOf` _ = Rule $  fail "Fixer.isSuperSetOf: You cannot modify a constant value"
UnionValue {} `isSuperSetOf` _ = Rule $  fail "Fixer: You cannot modify a union value"



-- | the function must satisfy the rule that if a >= b then f(a) implies f(b)
conditionalRule :: Fixable a => (a -> Bool) -> Value a -> Rule -> Rule
conditionalRule cond v (Rule act) = Rule $ addAction v (\x -> if cond x then act else return ())


dynamicRule  :: Fixable a =>  Value a -> (a -> Rule) -> Rule
dynamicRule v dr = Rule $ addAction v (unRule . dr)


propagateValue :: (HasBottom a, Fixable a) => a -> RvValue a -> IO ()
propagateValue p v =
  unless (isBottom p) $ do
    modifyIORef (todo $ fixer v) (Set.insert $ MkFixable v)
    modifyIORef (pending v) (join p)


-- | read result, calculating fixpoint if needed
readValue :: (Fixable a, MonadIO m, LOG.MonadLogging m) => Value a -> m a
readValue (IV v) = findFixpoint Nothing (fixer v) >> liftIO (readIORef (current v))
readValue (IOValue iov) = liftIO iov >>= readValue
readValue (ConstValue v) = return v
readValue (UnionValue a b) = join <$> readValue a <*> readValue b


readRawValue :: (Fixable a,MonadIO m) => Value a -> m a
readRawValue (IV v) = liftIO $ do
    readIORef (current v)
readRawValue (IOValue iov) = liftIO iov >>= readRawValue
readRawValue (ConstValue v) = return v
readRawValue (UnionValue a b) = liftIO $ do
    a' <- readRawValue a
    b' <- readRawValue b
    return (join a' b')



calcFixpoint :: (MonadIO m, LOG.MonadLogging m) => String -> Fixer -> m ()
calcFixpoint s fixer = findFixpoint (Just s) fixer



-- | find fixpoint, perhaps printing debugging information to specified handle. will not print anything if no calculation needed.
findFixpoint :: forall m. (MonadIO m, LOG.MonadLogging m) => Maybe String -> Fixer -> m ()
findFixpoint (fromMaybe "" -> mstring) Fixer{..} = do
  to <- liftIO $ readIORef todo
  unless (Set.null to) $ do
    let f [] !tl !n | n > 0, tl /= 0 = do
            vs <- liftIO $ readIORef todo
            liftIO $ writeIORef todo Set.empty
            LOG.logInfoM "findFixpoint" $  fromString $ "(" <> show n <> ")"
            f (Set.toList vs) (tl - 1) 0
        f [] _ n | n > 0 = LOG.logErrorM "findFixpont" "Aborting fixpoint solver." >> return ()
        f [] _ _ = return ()
        f (MkFixable v:vs) tl n = do
            p <- liftIO $ readIORef (pending v)
            c <- liftIO $ readIORef (current v)
            let diff = p `meet` c
            --if isBottom diff then f vs n else do
            if p `lte` c
              then f vs tl n
              else do
                liftIO $ do
                  as <- readIORef (action v)
                  writeIORef (current v) (p `join` c)
                  writeIORef (pending v) bottom
                  mapM_ ($ diff) as
                f vs tl (n + 1)
    LOG.logInfoM "findFixpoint" $ fromString $ "Finding fixpoint for " <> mstring <> ": [" <> show (Set.size to) <> "]"
    f (Set.toList to) (-1::Int) (0::Int)


-- vim: ts=8 sw=2 expandtab :

