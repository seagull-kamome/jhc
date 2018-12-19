-- simple constraint solver based on ideas from 'Once upon a polymorphic type' paper.
module Jhc.Solver.UnionSolve (
    C(),
    solve,
    Result(..),
    cAnnotate,
    islte,isgte,equals,
    (@<=),(@>=),(@=),(@<=@),(@>=@),(@=@)
    ) where

import Control.Monad(unless, forM, forM_)
import Data.Maybe (isNothing)
import Data.List(intersperse)
import Data.Monoid
import qualified Data.Foldable as S
import qualified Data.Map as Map
import qualified Data.Sequence as S
import qualified Data.Set as Set

import Jhc.Solver.Fixer.Fixable (Fixable(..))
import Jhc.Solver.UnionFind.IO as UF


-- ---------------------------------------------------------------------------

-- arguments are the lattice and the variable type
-- mappended together when used in a writer monad.
-- (C l v) represents a constraint (or set of constraints) that confine the
-- variables 'v' to within specific values of 'l'

newtype C l v = C (S.Seq (CL l v)) deriving (Semigroup, Monoid)

data Op = OpLte | OpEq | OpGte
instance Show Op where
  show OpEq  = " = "
  show OpGte = " >= "
  show OpLte = " <= "

data CL l v = CV v Op v | CL v Op l | CLAnnotate String (CL l v)

cAnnotate :: String -> C l v -> C l v
cAnnotate s (C seq) = C (fmap (CLAnnotate s) seq)

instance (Show e,Show l) => Show (C l e) where
    showsPrec _ (C xs) = showString "" . foldr (.) id (intersperse (showString "\n") (map shows (S.toList xs))) . showString "\n"

instance (Show e,Show l) => Show (CL l e) where
    showsPrec _ x = case x of
        CV v1 op v2 -> shows v1 . shows op . shows v2
        CL v1 op v2 -> shows v1 . shows op . shows v2
        CLAnnotate _ c -> shows c

bool t f b = if b then t else f

-- operator constraits, the @ is on the side that takes a variable.
v @<= l = cL v OpLte l
v @>= l = cL v OpGte l
v @=  l = cL v OpEq l
v @<=@ l = cV v OpLte l
v @>=@ l = cV v OpGte l
v @=@  l = cV v OpEq l

cL x y z = C (S.singleton (CL x y z))
cV x y z = C (S.singleton (CV x y z))

-- basic constraints
islte,isgte,equals :: (Fixable l,Ord v) => Either v l -> Either v l -> C l v
islte (Left v1) (Left v2)   = C (S.singleton (CV v1 OpLte v2))
islte (Left v1) (Right v2)  = C (S.singleton (CL v1 OpLte v2))
islte (Right v1) (Left v2)  = C (S.singleton (CL v2 OpGte v1))
islte (Right l1) (Right l2) = bool mempty (error $ "invalid constraint: " ++ showFixable l1 ++ " <= " ++ showFixable l2) (l1 `lte` l2)

isgte (Left v1) (Left v2)   = C (S.singleton (CV v1 OpGte v2))
isgte (Left v1) (Right v2)  = C (S.singleton (CL v1 OpGte v2))
isgte (Right v1) (Left v2)  = C (S.singleton (CL v2 OpLte v1))
isgte (Right l1) (Right l2) = bool mempty (error $ "invalid constraint: " ++ showFixable l1 ++ " >= " ++ showFixable l2) (l2 `lte` l1)

equals (Left v1) (Left v2)   = C (S.singleton (CV v1 OpEq v2))
equals (Left v1) (Right v2)  = C (S.singleton (CL v1 OpEq v2))
equals (Right v1) (Left v2)  = C (S.singleton (CL v2 OpEq v1))
equals (Right l1) (Right l2) = bool mempty (error $ "invalid constraint: " ++ showFixable l1 ++ " = " ++ showFixable l2) (l1 `eq` l2)

-- a variable is either set to a value or bounded by other values
data R l a = R l |  Ri (Maybe l) (Set.Set (RS l a))  (Maybe l) (Set.Set (RS l a))
    deriving(Show)
type RS l a = Element (R l a) a



-- replace variables with UnionFind elements
prepareConstraints :: Ord v => C l v -> IO ([CL l (RS l v)], Map.Map v (RS l v))
prepareConstraints (C cseq) = f Map.empty (S.toList cseq) id []
  where
    f m (c:cs) ar rs = do
        let h x mp = case Map.lookup x mp of
                Just v -> return (v,mp)
                Nothing -> do
                    v <- UF.new (Ri Nothing mempty Nothing mempty) x
                    return (v, Map.insert x v mp)
        case c of
            CL x op l -> do
                (x',m') <- h x m
                f m' cs id (ar (CL x' op l):rs)
            CV x op y -> do
                (x',m') <- h x m
                (y',m'') <- h y m'
                f m'' cs id (ar (CV x' op y'):rs)
            CLAnnotate s c -> f m (c:cs) (ar . CLAnnotate s) rs
    f m [] _ rs = return (rs,m)



check op x y = case op of
    OpEq -> x `eq` y
    OpLte -> x `lte` y
    OpGte -> y `lte` x



{-# NOINLINE solve #-}
solve :: (Fixable l, Show l, Show v, Ord v)
    => (String -> IO ())
    -> C l v
    -> IO (Map.Map v v,Map.Map v (Result l v))
solve putLog csp = do
    (pcs,varMap) <- prepareConstraints csp
    let procVar (CV x op y) = do
            xe <- UF.find x
            UF.find y >>= doVar "" xe op
        procVar (CLAnnotate s CL {}) =  return ()
        procVar CL {} = return ()
        procVar (CLAnnotate s cr) =  putLog s >>  procVar cr
        --
        doVar _ xe _ ye | xe == ye = return ()
        doVar lvl xe op ye = do
            putLog $ lvl ++ "Constraining: " ++ show (fromElement xe) ++ show op ++ show (fromElement ye)
            xw <- UF.getWeight xe
            yw <- UF.getWeight ye
            case (xw, yw) of
                (Ri xml xlb xmu xub,Ri yml ylb ymu yub) -> do
                    xub <- finds xub
                    xlb <- finds xlb
                    yub <- finds yub
                    ylb <- finds ylb
                    case op of
                        OpEq  ->  doEq lvl xe (Ri xml xlb xmu xub) ye (Ri yml ylb ymu yub)
                        OpLte -> doLte lvl xe (Ri xml xlb xmu xub) ye (Ri yml ylb ymu yub)
                        OpGte -> doLte lvl ye (Ri yml ylb ymu yub) xe (Ri xml xlb xmu xub)
                _ -> fail $ "UnionSolve: bad " ++  show (xw,yw)
        doEq lvl xe ~(Ri _ xlb _ xub) ye ~(Ri _ ylb _ yub) = do
            union const xe ye
            ne <- find xe
            nlb <- finds (xlb `Set.union` ylb)
            nub <- finds (yub `Set.union` xub)
            UF.putWeight ne (Ri Nothing nlb Nothing nub)
            checkRS lvl ne
        doLte lvl xe ~xw@(Ri xml xlb xmu xub) ye ~yw@(Ri yml ylb ymu yub) = do
            let done = UF.putWeight xe (Ri xml xlb xmu xub) >> UF.putWeight ye (Ri yml ylb ymu yub)
            if
              | ye `Set.member` xub || xe `Set.member` ylb -> done
              | ye `Set.member` xlb || xe `Set.member` yub -> doEq lvl xe xw ye yw
              | otherwise -> do
                  UF.putWeight xe (Ri xml xlb xmu (Set.insert ye (xub `Set.union` yub)))
                  UF.putWeight ye (Ri yml (Set.insert xe (ylb `Set.union` xlb)) ymu yub)
                  checkRS lvl xe
                  find ye >>= checkRS lvl
        checkRS lvl ve = do
            Ri l lb h ub <- UF.getWeight ve
            lb <- finds lb
            ub <- finds ub
            UF.putWeight ve (Ri l (Set.delete ve lb) h (Set.delete ve ub))
            let equiv = lb `Set.intersection` ub
             in forM_ (Set.toList equiv) $ doVar ('#':lvl) ve OpEq
        finds set = Set.fromList <$> mapM UF.find (Set.toList set)
    mapM_ procVar pcs

    let procLit (CL x op y) = do
            xe <- UF.find x
            doOp "" xe op y
        procLit (CLAnnotate s CV {}) =  return ()
        procLit CV {} = return ()
        procLit (CLAnnotate s cr) =  putLog s >>  procLit cr

        doOp lvl ve op l = do
            let doOp' = doOp ('-':lvl)
            putLog $ lvl ++ "Constraining: " ++ show (fromElement ve) ++ show op ++ show l
            vw <- getWeight ve
            case (op,vw) of
                (_,R c) | check op c l -> return ()
                        | otherwise -> fail $ "UnionSolve: constraint doesn't match (" ++ show c ++ show op ++ show l ++ ") when setting " ++ show (fromElement ve)
                (OpEq,Ri ml lb mu ub) | maybe True (`lte` l) ml && maybe True (l `lte`) mu -> do
                    updateWeight (const (R l)) ve
                    mapM_ (\v -> doOp' v OpLte l) (Set.toList lb)
                    mapM_ (\v -> doOp' v OpGte l) (Set.toList ub)
                (OpEq,_) -> fail $ "UnionSolve: setValue " ++ show (fromElement ve,vw,l)
                (OpLte,Ri _ _ (Just n) _) | n `lte` l -> return ()
                (OpGte,Ri (Just n) _ _ _) | l `lte` n -> return ()
                (OpLte,Ri (Just n) _ _ _) | n `eq` l -> doOp' ve OpEq l
                (OpGte,Ri _ _ (Just n) _) | n `eq` l -> doOp' ve OpEq l
                (OpLte,Ri (Just n) _ _ _) | l `lte` n -> fail $ "UnionSolve: lower than lower bound  " ++ show (fromElement ve,vw,l,n)
                (OpGte,Ri _ _ (Just n) _) | n `lte` l -> fail $ "UnionSolve: higher than higher bound  " ++ show (fromElement ve,vw,l,n)
                (OpLte,Ri ml lb mu ub) -> do
                    let nv@(Just l') = meet l <$> mu
                    doUpdate (Ri ml lb nv ub) ve
                    unless (nv `eq` mu) $
                        mapM_ (\v -> doOp' v OpLte l') (Set.toList lb)
                (OpGte,Ri ml lb mu ub) -> do
                    let nv@(Just l') = join l <$> ml
                    doUpdate (Ri nv lb mu ub) ve
                    unless (nv `eq` ml) $
                        mapM_ (\v -> doOp' v OpGte l') (Set.toList ub)
                -- _ -> fail $ "UnionSolve: bad " ++  show (fromElement ve,vw,op,l)
        --
        checkRS (Ri (Just l) _ (Just u) _) xe | l `eq` u = do
            putLog $ "Boxed in value of " ++ show (fromElement xe) ++ " being set to " ++ show l
            doOp "&" xe OpEq l
        checkRS (Ri (Just l) _ (Just u) _) _ | u `lte` l = fail "checkRS: you crossed the streams"
        checkRS (Ri (Just l) _ _ _) xe  | isTop l = do
            putLog $ "Going up:   " ++ show (fromElement xe)
            doOp "&" xe OpEq l
        checkRS (Ri  _ _ (Just u) _) xe | isBottom u = do
            putLog $ "Going down: " ++ show (fromElement xe)
            doOp "&" xe OpEq u
        checkRS _ xe = return ()
        --
        doUpdate r xe = updateWeight (const r) xe >> checkRS r xe
    mapM_ procLit pcs
    rs <- forM (Map.toList varMap) $ \ (a, e) -> do
        e' <- find e
        w <- getWeight e'
        let aa = fromElement e'
        rr <- case w of
            R v -> return (ResultJust (fromElement e') v)
            Ri ml lb mu ub -> do
                ub' <- map fromElement . Set.toList <$> finds ub
                lb' <- map fromElement . Set.toList <$> finds lb
                return (ResultBounded { resultRep = aa, resultUB = mu, resultLB = ml, resultLBV = lb', resultUBV = ub' })
        return ((a, aa), (aa, rr))
    return $ let (ma,mb) = unzip rs in (Map.fromList ma, Map.fromList mb)

-----------------------------------------------------------
-- The data type the results of the analysis are placed in.
-----------------------------------------------------------
data Result l a
  = ResultJust { resultRep :: a, resultValue :: l }
  | ResultBounded {
      resultRep :: a,
      resultLB :: Maybe l,
      resultUB :: Maybe l,
      resultLBV ::[a],
      resultUBV ::[a]
    }

instance (Show l, Show a) => Show (Result l a) where
  show (ResultJust a l) = show a ++ " = " ++ show l
  show ResultBounded {..} = sb resultLB resultLBV ++ " <= " ++ show resultRep ++ " <= " ++ sb resultUB resultUBV
    where
      sb x [] = maybe "_" show x
      sb x n = maybe mempty show x ++ show n

-- vim: ts=8 sw=2 expandtab :

