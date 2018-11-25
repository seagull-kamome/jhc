-- straightforward linear time solver for boolean constraints.
{-# LANGUAGE MultiWayIf #-}
module Util.BooleanSolver(
    CA(),
    CV(..),
    C(),
    fromCA,
    readValue,
    groundConstraints,
    processConstraints,
    Result(..),
    mkCA,
    equals,
    implies
    )where

import Control.Monad
import Data.IORef
import Control.Monad.Trans
import Data.List(intersperse)
import Data.Monoid
import Data.Typeable
import qualified Data.Set as Set
import qualified Data.Map as Map

--import Util.UnionFind
import Util.UnionFind as UF
-- import Data.FunctorM

-- ---------------------------------------------------------------------------

data CV v = CFalse | CTrue | CJust v deriving(Eq, Ord)
instance Functor CV where
  fmap f (CJust x) = CJust (f x)
  fmap _ x = x
instance Show v => Show (CV v) where
  show (CJust v) = show v
  show CTrue ="T"
  show CFalse = "F"



-- ---------------------------------------------------------------------------

data CL v = CV v `Cimplies` CV v deriving(Eq, Ord)
instance Functor CL where
  fmap f (x `Cimplies` y) = fmap f x `Cimplies` fmap f y
instance Show e => Show (CL e) where
  show (CJust x `Cimplies` CJust y) = "(" ++ show x ++ " -> " ++ show y ++ ")"
  show (CTrue `Cimplies` CJust y) = "(" ++ show y ++ " := T)"
  show (CJust x `Cimplies` CFalse) = "(" ++ show x ++ " := F)"
  show (x `Cimplies` y) = "(" ++ show x ++ " -> " ++ show y ++ ")"


-- ---------------------------------------------------------------------------

newtype C v = C [CL v] deriving (Semigroup, Monoid, Functor)
instance Show l => Show (C l) where
  show (C xs) = "(" ++  intercalate "," (map show l) ++ ")"

implies,equals :: CV v -> CV v -> C v
implies x y = C [x `Cimplies` y]
equals x y = C [x `Cimplies` y, y `Cimplies` x]


-- ---------------------------------------------------------------------------

-- a variable is either set to a value or bounded by other values
data Ri a = Ri (Set.Set (RS a))  (Set.Set (RS a))
type R a = CV (Ri a)
type RS a = (Element (R a) a)
newtype CA v = CA (RS v)

fromCA :: CA v -> v
fromCA (CA e) = fromElement e



readValue :: MonadIO m => CA v -> m (Result (CA v))
readValue (CA v) = liftIO $ do
    v <- find v
    w <- getW v
    case w of
        CTrue -> return ResultJust { resultValue = True }
        CFalse -> return ResultJust { resultValue = False }
        (CJust (Ri x y)) -> do
            x <- findSet x
            y <- findSet y
            return (ResultBounded (CA v) (map CA $ Set.toList x) (map CA $ Set.toList y))



findSet :: Set.Set (Element a b) -> IO (Set.Set (Element a b))
findSet xs = mapM find (Set.toList xs) >>= return . Set.fromList


mkCA :: MonadIO m => v -> m (CA v)
mkCA v = do liftM CA $ new (CJust (Ri mempty mempty)) v


groundConstraints :: (MonadIO m,Ord v) => C v -> m (C (CA v), Map.Map v (CA v))
groundConstraints (C cs) = liftIO $ do
    ref <- newIORef mempty
    let ccs = cs []
        nv v = do
            r <- readIORef ref
            case Map.lookup v r of
                Just v -> return v
                Nothing -> do
                    e <- liftM CA $ new (CJust (Ri mempty mempty)) v
                    writeIORef ref (Map.insert v e r)
                    return e
    v <- fmapM (fmapM nv) ccs
    rr <- readIORef ref
    return (C (v ++),rr)



processConstraints :: (Show v,MonadIO m)
    => Bool      -- ^ whether to propagate subset/superset info. if you only care about fixed results you don't need to do this. if you care about residual constraints and equivalance classes after solving then you should set this.
    -> C (CA v)  -- ^ the input
    -> m ()
processConstraints propagateSets (C cs) = mapM_ prule (cs []) where
    prule (CFalse `Cimplies` _) = return ()
    prule (_ `Cimplies` CTrue) = return ()
    prule (CTrue `Cimplies` CFalse) = fail "invalid constraint: T -> F"
    prule (CTrue `Cimplies` CJust (CA y)) = find y >>= set Nothing True
    prule (CJust (CA x) `Cimplies` CFalse) = find x >>= set Nothing False
    prule (CJust (CA x) `Cimplies` CJust (CA y)) = whrn (x /= y) pure pimp <*> find x <*> find y
    pimp' :: (MonadIO m,Show a) => RS a -> RS a -> m ()
    pimp' x y = do x <- find x; y <- find y; pimp x y
    pimp x y = when (x /= y) $ do
        xv <- getW x
        yv <- getW y
        case (xv,yv) of
            (CJust ra,CJust rb) -> liftIO $ implies x y ra rb
            (CFalse,_) -> return ()
            (_,CTrue) -> return ()
            (CTrue,CFalse) -> fail $ "invalid constraint T -> F: " ++ show x ++ " -> " ++ show y
            (CTrue,CJust _) -> set (Just x) True y
            (CJust _,CFalse) -> set (Just y) False x

    set mu b xe = do
        w <- getW xe
        case (w,b) of
            (CTrue,True) -> return ()
            (CFalse,False) -> return ()
            (CJust (Ri _ sh),True) -> do putW xe CTrue; mapM_ (set mu True) (Set.toList sh)
            (CJust (Ri sl _),False) -> do putW xe CFalse; mapM_ (set mu False) (Set.toList sl)
            _ -> fail $ "invalid constrant: " ++ show xe ++ " := " ++ show b
        fmapM_ (union const xe) mu

    implies :: (MonadIO m,Show a) => RS a -> RS a -> Ri a -> Ri a -> m ()
    implies xe ye ra rb = do
        ra@(Ri xl xh) <- findRi xe ra
        rb@(Ri yl yh) <- findRi ye rb
        if
          | xe `Set.member` yh -> liftIO $ equals xe ye ra rb
          | xe `Set.member` yl -> return ()
          | ye `Set.member` xl -> liftIO $ equals xe ye ra rb
          | ye `Set.member` xh -> return ()
          | otherwise -> do
              putW xe (CJust $ Ri xl (Set.insert ye xh))
              putW ye (CJust $ Ri (Set.insert xe yl) yh)
              when propagateSets $ mapM_ (pimp' xe) (Set.toList yh)
              when propagateSets $ mapM_ (flip pimp' ye) (Set.toList xl)
              return ()
    findRi x (Ri l h) = do
        l <- liftM Set.fromList (mapM find (Set.toList l))
        h <- liftM Set.fromList (mapM find (Set.toList h))
        return (Ri l h)
    equals xe ye (Ri xl xh) (Ri yl yh) = do
        let nl = (xl `mappend` yl)
        let nh = (xh `mappend` yh)
        union (\ _ _ -> CJust (Ri nl nh)) xe ye
        when propagateSets $ do
            Ri nl nh <- findRi xe (Ri nl nh)
            putW xe (CJust $ Ri nl nh)
            let eq = Set.intersection nl nh
            flip mapM_ (Set.toList eq) $ \ne -> do
                ne <- find ne
                CJust ri <- getW ne
                ri <- findRi ne ri
                equals xe ne (Ri nl nh) ri
            return ()
        return () :: IO ()




data Result a =
    ResultJust {
        resultValue :: Bool
    }
    | ResultBounded {
        resultRep :: a,
        resultLB ::[a],
        resultUB ::[a]
    }


instance Functor Result where
    fmap f (ResultBounded x ys zs) = ResultBounded (f x) (map f ys) (map f zs)
    fmap f (ResultJust x) = ResultJust x

instance (Show a) => Show (Result a) where
    showsPrec _ x = (showResult x ++)

showResult (ResultJust l) = show l
showResult rb@ResultBounded {} = sb (resultLB rb) ++ " <= " ++ show (resultRep rb) ++ " <= " ++ sb (resultUB rb)   where
    sb n | null n = "_"
    sb n = show n



collectVars (Cimplies x y:xs) = x:y:collectVars xs
collectVars [] = []

