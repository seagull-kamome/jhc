module Language.Grin.Noodle where

-- various routines for manipulating and exploring grin code.

import Control.Monad.Writer
import qualified Data.Set as Set

import C.Prims
import Debug.Trace
import Grin.Grin
import Options(flint)
import StringTable.Atom(Atom())
import Support.CanType
import Support.FreeVars
import Support.Tickle
import Util.GMap
import Util.Gen
import Util.HasSize
import Util.SetLike

modifyTail :: Lam -> Exp -> Exp
modifyTail lam@(_ :-> lb) te = f (sempty :: GSet Atom) te where
    lamFV = freeVars lam :: GSet Var
    f lf e | False && trace ("modifyTail: " ++ show (lf,e)) False = undefined
    f _ (Error s ty) = Error s (getType lb)
    f lf (Case x ls) = Case x (map (g lf) ls)
    f _ lt@Let {expIsNormal = False } = lt :>>= lam
    f lf lt@Let {expDefs = defs, expBody = body, expIsNormal = True } = updateLetProps lt { expBody = f nlf body, expDefs = defs' } where
        nlf = lf `union` fromList (map funcDefName defs)
        defs' = [ updateFuncDefProps d { funcDefBody = g nlf (funcDefBody d) } | d <- defs ]
    f lf lt@MkCont {expLam = lam, expCont = cont } = lt { expLam = g lf lam, expCont = g lf cont }
    f lf (e1 :>>= p :-> e2) = e1 :>>= p :-> f lf e2
    f lf e@(App a as t) | a `member` lf = App a as (getType lb)
    f lf e = e :>>= lam
    g lf (p :-> e) | flint && not (isEmpty $ intersection (freeVars p) lamFV) = error "modifyTail: lam floated inside bad scope"
    g lf (p :-> e) = p :-> f lf e

instance Tickleable Exp Lam where
    tickleM = mapBodyM
instance Tickleable Exp Exp where
    tickleM = mapExpExp
instance Tickleable Val Exp where
    tickleM = mapExpVal
instance Tickleable Val Val where
    tickleM = mapValVal
    tickleM_ = mapValVal_
instance Tickleable Lam Grin where
    tickleM f grin = liftM (`setGrinFunctions` grin) $ mapM  (\x -> do nb <- f (funcDefBody x); return (funcDefName x, nb)) (grinFunctions grin)
instance Tickleable Lam FuncDef where
    tickleM f fd = funcDefBody_uM f fd
instance Tickleable (Atom,Lam) FuncDef where
    tickleM f fd@FuncDef { funcDefName = n, funcDefBody = b } = do
    (n',b') <- f (n,b)
    return $  updateFuncDefProps fd { funcDefBody = b', funcDefName = n' }



--------------------------
-- examining and reporting
--------------------------

isManifestNode :: Monad m => Exp -> m [Atom]
isManifestNode e = f (sempty :: GSet Atom) e where
    f lf _ | False && trace ("isManifestNode: " ++ show lf) False = undefined
    f lf (Return [(NodeC t _)]) = return [t]
    f lf Error {} = return []
    f lf (App a _ _) | a `member` lf = return []
    f lf Let { expBody = body, expIsNormal = False } = f lf body
    f lf Let { expBody = body, expDefs = defs, expIsNormal = True } = ans where
        nlf = lf `union` fromList (map funcDefName defs)
        ans = do
            xs <- mapM (f nlf . lamExp . funcDefBody) defs
            b <- f nlf body
            return (concat (b:xs))
    f lf (Case _ ls) = do
        cs <- Prelude.mapM (f lf) [ e | _ :-> e <- ls ]
        return $ concat cs
    f lf (_ :>>= _ :-> e) = isManifestNode e
    f lf _ = fail "not manifest node"

-- collect tail and normally called functions
-- expression (tail called, non tail called)
collectFuncs :: Exp -> (Set.Set Atom,Set.Set Atom)
collectFuncs exp = runWriter (cfunc exp) where
        clfunc (l :-> r) = cfunc r
        cfunc e | False && trace ("isManifestNode: " ++ show e) False = undefined
        cfunc (e :>>= v :-> op@(BaseOp _ v')) | isNop op && v == v' = do cfunc e
        cfunc (e :>>= y) = do
            xs <- cfunc e
            tell xs
            clfunc y
        cfunc (App a _ _) = return (singleton a)
        cfunc (Case _ as) = do
            rs <- mapM clfunc as
            return (mconcat rs)
        cfunc Let { expFuncCalls = (tail,nonTail) } = do
            tell nonTail
            return tail
        cfunc Error {} = return mempty
        cfunc Prim {} = return mempty
        cfunc Return {} = return mempty
        cfunc BaseOp {} = return mempty
        cfunc Alloc {} = return mempty
        cfunc GcRoots { expBody = b} = cfunc b
        cfunc NewRegion { expLam = l } = clfunc l
        cfunc MkCont { expCont = l1, expLam = l2 } = do
            a <- clfunc l1
            b <- clfunc l2
            return (a `mappend` b)
        cfunc x = error "Grin.Noodle.collectFuncs: unknown"


data ReturnInfo = ReturnNode (Maybe Atom,[Ty]) | ReturnConst Val | ReturnCalls Atom | ReturnOther | ReturnError
    deriving(Eq,Ord)

getReturnInfo :: Exp -> [ReturnInfo]
getReturnInfo  e = ans where
    ans = execWriter (f (sempty :: GSet Atom) e)
    tells x = tell [x]
    f lf (Return [(NodeC t as)]) = tells (ReturnNode (Just t,map getType as))
    f lf (Return [z]) | valIsConstant z = tell [ReturnConst z]
    f lf Error {} = tells ReturnError
    f lf (Case _ ls) = do Prelude.mapM_ (f lf) [ e | _ :-> e <- ls ]
    f lf (_ :>>= _ :-> e) = f lf e
    f lf Let { expBody = body, expIsNormal = False } = f lf body
    f lf (App a _ _) | a `member` lf = return ()
    f lf Let { expBody = body, expDefs = defs, expIsNormal = True } = ans where
        nlf = lf `union` fromList (map funcDefName defs)
        ans = do
            mapM_ (f nlf . lamExp . funcDefBody) defs
            f nlf body
    f _ (App a _ _) = tells $ ReturnCalls a
    f _ e = tells ReturnOther

mapGrinFuncsM :: Monad m => (Atom -> Lam -> m Lam) -> Grin -> m Grin
mapGrinFuncsM f grin = liftM (`setGrinFunctions` grin) $ mapM  (\x -> do nb <- f (funcDefName x) (funcDefBody x); return (funcDefName x, nb)) (grinFunctions grin)
