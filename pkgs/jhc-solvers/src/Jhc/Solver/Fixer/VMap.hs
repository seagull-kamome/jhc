module Jhc.Solver.Fixer.VMap(
    VMap(),
    Proxy(..),
    vmapSingleton,
    vmapArgSingleton,
    vmapArg,
    vmapValue,
    vmapMember,
    vmapProxyIndirect,
    vmapPlaceholder,
    vmapDropArgs,
    vmapHeads
    )where

import Data.Monoid(Monoid(..))
import Data.List(intercalate, sort, sortBy, group)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Typeable as T -- qualified to avoid clashing with T.Proxy

import GHC.Exts (IsString(fromString))

import Jhc.Solver.Fixer.Fixable (Fixable(..), HasBottom(..))



-- | General data type for finding the fixpoint of a general tree-like structure.

data VMap p n = VMap {
    vmapArgs    :: Map.Map (n,Int) (VMap p n),
    vmapNodes   :: Either (Proxy p) (Set.Set n)
    }
    deriving(T.Typeable)

data Proxy p = Proxy p | DepthExceeded
    deriving(Eq,Ord,T.Typeable)

instance Show p => Show (Proxy p) where
    show (Proxy p) = show p
    show DepthExceeded = "*"

emptyVMap :: (Ord p, Ord n) => VMap p n
emptyVMap = VMap { vmapArgs = mempty, vmapNodes = Right mempty }

vmapSingleton :: (Ord p, Ord n) => n -> VMap p n
vmapSingleton n = emptyVMap { vmapNodes = Right $ Set.singleton n }

vmapArgSingleton :: (Ord p,Ord n,Show p,Show n) => n -> Int -> VMap p n -> VMap p n
vmapArgSingleton n i v
    | isBottom v = emptyVMap
    | otherwise = pruneVMap $ emptyVMap { vmapArgs = Map.singleton (n,i) v }

vmapArg :: (Ord p,Ord n,Show p,Show n) => n -> Int -> VMap p n -> VMap p n
vmapArg n i vm@VMap {..} = case Map.lookup (n,i) vmapArgs of
    Just x -> x `join` vmapProxyIndirect i vm
    Nothing -> vmapProxyIndirect i vm

vmapProxyIndirect :: (Show p,Show n,Ord p,Ord n,Fixable (VMap p n)) => Int -> VMap p n -> VMap p n
vmapProxyIndirect _ VMap { vmapNodes = Left l } = emptyVMap { vmapNodes = Left l }
vmapProxyIndirect _ _ = emptyVMap

vmapValue :: (Show p,Show n,Ord p,Ord n) => n -> [VMap p n] -> VMap p n
vmapValue n xs = pruneVMap VMap { vmapArgs = Map.fromAscList (zip (zip (repeat n) [0..]) xs), vmapNodes = Right $ Set.singleton n }

vmapPlaceholder :: (Show p,Show n,Ord p,Ord n) => p  -> VMap p n
vmapPlaceholder p = emptyVMap { vmapNodes = Left (Proxy p) }

vmapDropArgs :: Ord n => VMap p n -> VMap p n
vmapDropArgs vm = vm { vmapArgs = mempty }

vmapHeads :: Monad m => VMap p n -> m [n]
vmapHeads VMap { vmapNodes = Left _ } = fail "vmapHeads: VMap is unknown"
vmapHeads VMap { vmapNodes = Right set } = return $ Set.toList set

vmapMember :: Ord n => n -> VMap p n -> Bool
vmapMember _ VMap { vmapNodes = Left _ } = True
vmapMember n VMap { vmapNodes = Right set } = n `Set.member` set

pruneVMap :: (Ord n,Ord p,Show n,Show p) => VMap p n -> VMap p n
pruneVMap vmap = f (7::Int) vmap where
    f 0 _ = emptyVMap { vmapNodes = Left DepthExceeded }
    f _ VMap { vmapNodes = Left p} = emptyVMap {vmapNodes = Left p}
    f n x@VMap{ vmapArgs = map} = x { vmapArgs = map' } where
        map' = Map.filter g (Map.map (f (n - 1)) map)
        g vs = not $ isBottom vs

instance (Ord p,Ord n,Show p,Show n) => Show (VMap p n) where
  show VMap{ vmapNodes = Left p } = show p
  show VMap{ vmapArgs = n, vmapNodes = Right s } =
    "{" ++ (intercalate ",\n" $ (map f $ snub $ (map fst $ Map.keys n) ++ Set.toList s)) ++ "}"
    where
      f a = (if Set.member a s then show a else "#" <> show a)
            <> (if null (g a) then mempty else show (g a))
      g a = sortUnder fst [ (i,v) | ((a',i),v) <- Map.toList n, a' == a ]
      sortUnder f = sortBy (\x y -> f x `compare` f y)
      snub = map head . group . sort




instance (Ord p, Ord n) => HasBottom (VMap p n) where bottom = emptyVMap
instance (Show p,Show n,Ord p,Ord n) => Fixable (VMap p n) where
    isBottom VMap { vmapArgs = m, vmapNodes = Right s } = Map.null m && Set.null s
    isBottom _ = False
    join x y | x `lte` y = y
    join x y | y `lte` x = x
    join VMap { vmapNodes = Left p } _ = emptyVMap { vmapNodes = Left p }
    join _ VMap { vmapNodes = Left p } = emptyVMap { vmapNodes = Left p }
    join VMap { vmapArgs = as, vmapNodes = Right ns } VMap { vmapArgs = as', vmapNodes = Right ns'} = pruneVMap $ VMap {vmapArgs = Map.unionWith join as as', vmapNodes = Right $ Set.union ns ns' }
    meet _ VMap { vmapNodes = Left _ } = bottom
    meet x@VMap { vmapNodes = Left _ } _ = x
    meet VMap { vmapArgs = n1, vmapNodes = Right w1} VMap { vmapArgs = n2, vmapNodes = Right w2 } = pruneVMap $ VMap { vmapArgs = Map.fromAscList $ [
            case Map.lookup (a,i) n2 of
                Just v' ->  ((a,i),v `meet` v')
                Nothing ->  ((a,i),v)
        | ((a,i),v) <- Map.toAscList n1 ], vmapNodes = Right (w1 Set.\\ w2) }
    lte _ VMap { vmapNodes = Left _ } = True
    lte VMap { vmapNodes = Left _ } _ = False
    lte x@VMap { vmapArgs = as, vmapNodes = Right ns } VMap { vmapArgs = as', vmapNodes = Right ns'} =  (Set.null (ns Set.\\ ns') && (Map.null $ Map.differenceWith (\a b -> if a `lte` b then Nothing else Just undefined) as as'))
    showFixable = fromString . show

instance (Show p, Show n, Ord p, Ord n) => Semigroup (VMap p n) where (<>) = join
instance (Show p, Show n, Ord p, Ord n) => Monoid (VMap p n) where mempty = bottom

-- vim: ts=8 sw=2 expandtab :

