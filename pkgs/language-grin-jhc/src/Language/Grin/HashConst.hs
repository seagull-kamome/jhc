module Language.Grin.HashConst(newConst,HcHash(),HcNode(..),toList,emptyHcHash) where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

import Language.Grin.Tag
import Language.Grin.Var
import Language.Grin.Val


-- ---------------------------------------------------------------------------


-- TODO tuples

data HcNode = HcNode !(Tag sym) [Either (Val sym primtypes primval) Int]
    deriving(Show,Ord,Eq)

data HcHash = HcHash !Int (Map.Map HcNode Int)
    deriving(Show)

emptyHcHash = HcHash 1 Map.empty

newConst :: MonadState HcHash m
         => Set.Set (Tag sym) -> Val sym primtypes primval -> m (Bool,Int)
newConst cpr = f where
    f (ValNodeC t vs) = do
        vs' <- forM vs $ \v -> case v of
          ValLit{}  -> return $ Left v
          ValPrim{} -> return $ Left v
          ValVar (Var n) _ | n < 0  -> return $ Left v
          ValConst (ValNodeC _ []) -> return $ Left v
          ValConst (ValNodeC a _) | a `Set.member` cpr -> return $ Left v
          ValConst n -> (Right . snd) <$> f n
          ValNodeC _ [] -> return $ Left v
          ValNodeC a _ | a `Set.member` cpr  -> return $ Left v
          ValNodeC{} -> (Right . snd) <$> f v
          e -> error $ "HashConst.g: " ++ show e
        let n' = HcNode t vs'
        HcHash c h <- get
        case Map.lookup n' h of
          Just n'' -> return (True,n'')
          Nothing -> do
            let h' = Map.insert n' c h
            put $ HcHash (c + 1) h'
            return (False,c)
    f _ = error "HashConst.newConst'"

toList :: HcHash -> [(HcNode,Int)]
toList (HcHash _ mp) = reverse ans where
    gr = newGraph (Map.toList mp) snd (gk . fst)
    gk (HcNode _ xs) = [ i | Right i <- xs]
    ans = topSort gr
