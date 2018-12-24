module Language.Grin.Optimizer.DeadCode(
  deadCode) where


import Data.Bool (bool)
import qualified Data.Set as Set
import qualified Data.EnumSet.EnumSet as ESet


import qualified Jhc.Logging as LOG
import Jhc.Solvers.Fixer.Fixer
import Jhc.Solvers.Fixer.Supply

import Language.Grin.AST.Tag
import Language.Grin.AST.Val
import Language.Grin.AST.Var
import Language.Grin.AST.Type
import Language.Grin.AST.Lambda
import Language.Grin.AST.Expression
import Language.Grin.AST.Program


-- ---------------------------------------------------------------------------

removeDeadArgs :: Set.Set (Tag sym)
               -> Set.Set (Tag sym)
               -> Set.Set Var
               -> Set.Set (Tag sym, Int)
               -> (Tag sym, Lambda)
               -> WhizState
               -> m (WhizState, (Tag sym, Lambda))
removeDeadArgs funSet directFuncs usedCafs usedArgs (a, l) whizState
    = whiz (\_ x -> x) (\(p, e) -> return (Just (p, f e))) (return . f) whizState (margs a l)
        >>= \(l,ws) -> return (ws,(a,l))
  where
    margs fn x@(Lambda as e) = bool x (Lambda (removeArgs fn as) e) $ Set.member a directFuncs
    --
    f (ExprApp fn as ty)  = ExprApp fn (map clearCaf $ dff fn as) ty
    f (ExprReturn [ValNodeC fn as])                   | Just fn' <- tagToFunction fn = ExprReturn [ValNodeC fn $ map clearCaf $ dff' fn' as]
    f (ExprBaseOp (StoreNode False) [ValNodeC fn as]) | Just fn' <- tagToFunction fn = ExprBaseOp (StoreNode False) [ValNodeC fn $ map clearCaf $ dff' fn' as]
    f (ExprBaseOp Overwrite [ValVar v TypINode ,_])  | deadCaf v = Return []
    f (ExprBaseOp Overwrite [p, ValNodeC fn as])      | Just fn' <- tagToFunction fn = ExprBaseOp Overwrite  [p,ValNodeC fn $ map clearCaf $ dff' fn' as]
    f lt@Let { expDefs = defs }  = updateLetProps lt { expDefs = defs' }
      where
        defs' = [ updateFuncDefProps df { funcDefBody = margs name body }
                     | df@FuncDef { funcDefName = name, funcDefBody = body } <- defs
                     , name `Set.member` funSet ]
    f x = return x
    --
    dff fn as = bool (dff'' fn as) (removeArgs fn as) $ Set.member fn directFuncs
    dff' fn as = bool (dff'' fn as) as $ Set.member fn directFuncs
    dff'' fn as = bool (zipWIth df as [0..]) as $ Set.notMember fn funSet -- if function was dropped, we don't have argument use information.
      where
        deadVal x = case x of { Lit 0 _ -> True; _ -> isHole x }
        df a i = if deadVal a || member (fn,i) usedArgs
                      then a else properHole (getType a)
    --
    clearCaf :: Val sym primtypes primval -> Val sym primtypes primval
    clearCaf (ValVar v TyINode) | deadCaf v = properHole TypINode
    clearCaf (ValNodeC a xs) = ValNodeC a $ map clearCaf xs
    clearCaf (ValIndex a b)  = ValIndex (clearCaf a) (clearCaf b)
    clearCaf (ValConst a)    = ValConst $ clearCaf a
    clearCaf x = x
    --
    deadCaf :: Var -> Bool
    deadCaf v = v < Var 0 && Set.notMember v usedCafs
    --
    removeArgs fn as = concat [ if Set.member (fn,i) usedArgs then a else error "perhapsM" | a <- as | i <- [0..] ]



implies :: Value Bool -> Value Bool -> Rule
implies x y = y `isSuperSetOf` x




-- | Remove dead code from Grin.
deadCode :: (MonadIO m, LOG.MonadLogging m)
         => [Tag sym]  -- ^ roots
         -> Program    -- ^ input
         -> m Program -- ^ output
deadCode roots grin = do
    fixer <- newFixer
    usedFuncs <- newSupply fixer
    usedArgs <- newSupply fixer
    usedCafs <- newSupply fixer
    pappFuncs <- newValue fixer bottom
    suspFuncs <- newValue fixer bottom
    -- set all roots as used
    forM_ roots $ \r -> addRule $ ConstValue True `implies` sValue usedFuncs r
    let postInline = phaseEvalInlined (grinPhase grin)

    forM_ (grinCafs grin) $ \ (v,NodeC t []) -> do
        (0,fn) <- tagUnfunction t
        v' <- supplyValue usedCafs v
        addRule $ conditionalRule id v' $ (suspFuncs `isSuperSetOf` ConstValue (singleton fn))
        addRule $ v' `implies` (sValue usedFuncs fn)

    mapM_ (go fixer pappFuncs suspFuncs usedFuncs usedArgs usedCafs postInline) (grinFuncs grin)
    findFixpoint Nothing {-"Dead Code"-} fixer
    --
    ua <- supplyReadValues usedArgs
    uc <- supplyReadValues usedCafs
    uf <- supplyReadValues usedFuncs
    pappFuncs <- readValue pappFuncs
    suspFuncs <- readValue suspFuncs
    let cafSet = fg uc
        funSet = fg uf
        argSet = fg ua
                 `union`
                 fromList [ (n,i) | FuncDef n (args :-> _) _ _ <- grinFunctions grin,
                                        n `member` grinEntryPoints grin,
                                        i <- [0 .. length args] ]
        directFuncs =  funSet \\ suspFuncs \\ pappFuncs
        fg xs = fromList [ x | (x,True) <- xs ]
    newCafs <- flip mconcatMapM (grinCafs grin) $ \ (x,y) -> if x `member` cafSet then return [(x,y)] else tick stats "Optimize.dead-code.caf" >> return []
    let f ((x,y):xs) rs ws = do
            if not $ x `member` funSet then tick stats "Optimize.dead-code.func" >> f xs rs ws else do
            (ws',r) <- runStatIO stats $ removeDeadArgs postInline funSet directFuncs cafSet argSet (x,y) ws
            f xs (r:rs) ws'
        f [] rs _ = return rs
    newFuncs <- f (grinFuncs grin) [] whizState
    --newFuncs <- flip mconcatMapM (grinFuncs grin) $ \ (x,y) -> do
    let (TyEnv mp) = grinTypeEnv grin
    mp' <- flip mconcatMapM (toList mp) $ \ (x,tyty@TyTy { tySlots = ts }) -> case Just x  of
        Just _ | tagIsFunction x, not $ x `member` funSet -> return []
        Just fn | fn `member` directFuncs -> do
            let da (t,i)
                    | member (fn,i) argSet = return [t]
                    | otherwise = tick stats ("Optimize.dead-code.arg-func.{" ++ show x ++ "-" ++ show i) >> return []
            ts' <- mconcatMapM da (zip ts [0..])
            return [(x,tyty { tySlots = ts' })]
        _ -> return [(x,tyty)]

    return $ setGrinFunctions newFuncs grin {
        grinCafs = newCafs,
        grinPartFunctions = pappFuncs,
        grinTypeEnv = TyEnv $ fromList mp',
        grinSuspFunctions = suspFuncs
        }



combineArgs :: a -> [b] -> [((a, Int), b)]
combineArgs fn as = [ ((fn, n), a) | (n,a) <- zip [0 :: Int ..] as]



go :: (MonadIO m, Collection b, Collection a, Fixable b, Fixable a,
       Elem b ~ Atom, Elem a ~ Atom) =>
      Fixer
      -> Value a
      -> Value b
      -> Supply Tag Bool
      -> Supply (Tag, Int) Bool
      -> Supply Var Bool
      -> Bool
      -> (Tag, Lam)
      -> m Lam
go fixer pappFuncs suspFuncs usedFuncs usedArgs usedCafs postInline (fn,as :-> body) = ans where
    goAgain = go fixer pappFuncs suspFuncs usedFuncs usedArgs usedCafs postInline
    ans = do
        usedVars <- newSupply fixer

        flip mapM_ (combineArgs fn as) $ \ (ap,Var v _) -> do
            x <- supplyValue usedArgs ap
            v <- supplyValue usedVars v
            addRule $ v `implies` x
        -- a lot of things are predicated on this so that CAFS are not held on to unnecesarily
        fn' <- supplyValue usedFuncs fn
        let varValue v | v < v0 = sValue usedCafs v
                       | otherwise = sValue usedVars v
            f e = g e >> return e
            g (BaseOp Eval [e]) =  addRule (doNode e)
            g (BaseOp Apply {} vs) =  addRule (mconcatMap doNode vs)
            g (Case e _) =  addRule (doNode e)
            g Prim { expArgs = as } = addRule (mconcatMap doNode as)
            g (App a vs _) = do
                addRule $ conditionalRule id fn' $ mconcat [ mconcatMap (implies (sValue usedArgs fn) . varValue) (freeVars a) | (fn,a) <- combineArgs a vs]
                addRule $ fn' `implies` sValue usedFuncs a
                addRule (mconcatMap doNode vs)
            g (BaseOp Overwrite [Var v _,n]) | v < v0 = do
                v' <- supplyValue usedCafs v
                addRule $ conditionalRule id v' $ doNode n
            g (BaseOp Overwrite [vv,n]) = addRule $ (doNode vv) `mappend` (doNode n)
            g (BaseOp PokeVal [vv,n]) = addRule $ (doNode vv) `mappend` (doNode n)
            g (BaseOp PeekVal [vv]) = addRule $ (doNode vv)
            g (BaseOp Promote [vv]) = addRule $ (doNode vv)
            g (BaseOp _ xs) = addRule $ mconcatMap doNode xs
            g Alloc { expValue = v, expCount = c, expRegion = r } = addRule $ doNode v `mappend` doNode c `mappend` doNode r
            g Let { expDefs = defs, expBody = body } = do
                mapM_ goAgain [ (name,bod) | FuncDef { funcDefBody = bod, funcDefName = name } <- defs]
                flip mapM_ (map funcDefName defs) $ \n -> do
                    --n' <- supplyValue usedFuncs n
                    --addRule $ fn' `implies` n'
                    return ()
            g Error {} = return ()
            -- TODO - handle function and case return values smartier.
            g (Return ns) = mapM_ (addRule . doNode) ns
            g x = error $ "deadcode.g: " ++ show x
            h' (p,e) = h (p,e) >> return (Just (p,e))
            h (p,BaseOp (StoreNode _) [v]) = addRule $ mconcat $ [ conditionalRule id  (varValue pv) (doNode v) | pv <- freeVars p]
            h (p,BaseOp Demote [v]) = addRule $ mconcat $ [ conditionalRule id  (varValue pv) (doNode v) | pv <- freeVars p]
            h (p,Alloc { expValue = v, expCount = c, expRegion = r }) = addRule $ mconcat $ [ conditionalRule id  (varValue pv) (doNode v `mappend` doNode c `mappend` doNode r) | pv <- freeVars p]
            h (p,Return vs) = mapM_ (h . \v -> (p,BaseOp Promote [v])) vs -- addRule $ mconcat $ [ conditionalRule id  (varValue pv) (doNode v) | pv <- freeVars p]
            h (p,BaseOp Promote [v]) = addRule $ mconcat $ [ conditionalRule id  (varValue pv) (doNode v) | pv <- freeVars p]
            h (p,e) = g e
            doNode (NodeC n as) | not postInline, Just (x,fn) <- tagUnfunction n  = let
                consts = (mconcatMap doNode as)
                usedfn = implies fn' (sValue usedFuncs fn)
                suspfn | x > 0 = conditionalRule id fn' (pappFuncs `isSuperSetOf` ConstValue (singleton fn))
                       | otherwise = conditionalRule id fn' (suspFuncs `isSuperSetOf` ConstValue (singleton fn))
                in mappend consts $ mconcat (usedfn:suspfn:[ mconcatMap (implies (sValue usedArgs fn) . varValue) (freeVars a) | (fn,a) <- combineArgs fn as])
            doNode x = doConst x `mappend` mconcatMap (implies fn' . varValue) (freeVars x)
            doConst _ | postInline  = mempty
            doConst (Const n) = doNode n
            doConst (NodeC n as) = mconcatMap doConst as
            doConst _ = mempty

        (nl,_) <- whiz (\_ -> id) h' f whizState (as :-> body)
        return nl


-- vim: ts=8 sw=2 expandtab :


