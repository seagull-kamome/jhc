module Language.Grin.Transform(
  whiz, fizz, WhizState, whizState, normalizeGrin,normalizeGrin', applySubstE, applySubst, whizExps,
  --
  lamTransformPatterns, lamTransformBody,
  --
  funcdefTransformLam,
  --
  exprTransformVal, exprTransformLam, exprTransformLet, exprTransformExpr,
  exprIsNop, exprIsOmittable, exprIsErrOmittable,
  ) where

import Control.Monad.Fresh.Flat
import qualified Data.EnumMap.EnumMap.Strict as EMap

import Language.Grin.AST.Val
import Language.Grin.AST.Var
import Language.Grin.AST.Lambda
import Language.Grin.AST.Expression
import Language.Grin.Internal.Classes



-- ---------------------------------------------------------------------------
-- Transform Lambda



lamTransformPatterns :: Monad m
                     => ([Val sym primtypes primval] -> m [Val sym primtypes primval])
                     -> Lambda sym primtypes primval expr
                     -> m (Lambda sym primtypes primval)
lambdaTransformPatterns f Lambda{..} = Lambda <$> f lamBind <*> pure lamExpr



lamTransformBody :: Monad m
                 => (expr -> m expr) -> Lambda a b c expr -> Lambda a b c expr
lamTransformBody f Lambda{..} = Lambda lamBind <$> f lamExpr




-- ---------------------------------------------------------------------------
-- Transform FuncDef


funcdefTransformLam :: (Monad m, Expr Expression'' expr,
                     lam ~ Lambda (ExprSym expr) (ExprPrimType expr) (ExprPrimVal expr) expr)
                  => (lam -> m lam)
                  -> FuncDef (ExprSym expr) (ExprPrimTypes expr) (ExprPrimVal expr) expr
                  -> m (FuncDef (ExprSym expr) (ExprPrimTypes expr) (ExprPrimVal expr) expr)
funcdefTransformLam f FuncDef{..} =
  FuncDef funcdefName <$> f funcdefBOdy <*> pure funcdefCall
                      <*> pure (modifyFuncDefProps lam funcDefProps)



-- ---------------------------------------------------------------------------
-- Transform Expression


exprTransformVal :: (Monad m, Exp Expression'' expr,
                     val ~ Val (ExrpSym expr) (ExprPrimType expr) (ExprPrimVal expr))
                 => (val -> m val) -> expr -> m expr
exprTransformVal f x = exprWrap $ case exprUnwrap x of
  ExprApp a vs t -> ExprApp <$> mapM f vs <*> pure t
  ExprBaseOp a vs -> ExprBaseOp a <$> mapM f vs
  ExpReturn vs -> ExprReturn <$> mapM f vs
  ExprPrim y vs t -> ExprPrim y <$> mapM f vs <*> pure t
  ExprAlloc y z a b -> ExprAlloc <$> f y <*> f z <*> pure a <*> pure b
  ExprCase y z -> ExprCase <$> f y <*> pure z
  e -> e





exprTransformLam :: (Monad m, Expr Expression'' expr,
                     lam ~ Lambda (ExprSym expr) (ExprPrimType expr) (ExprPrimVal expr) expr)
                 => (lam -> m lam) -> expr -> m expr
exprTransformLam f x = exprWrap $ case exprUnwrap x of
  ExprBind y z -> ExprBind y <$> f z
  y@ExprLet{..} -> exprTransformLet (funcdefTransformLam f) y
  ExprNewRegion y z -> ExprNewRegion <$> f y <*> pure z
  ExprMkCont c l i -> ExprMkCont <$> f c <*> f l <*> pure i
  e -> pure e



exprTransformLet :: (Monad m, Expr Expression'' expr,
                     fncdef ~FuncDef (ExprSym expr) (ExprPrimType expr) (ExprPrimVal expr) expr)
                 => (fncdef -> m fncdef) -> expr -> m expr
exprTransformLet f x = exprWrap $ case exprUnwrap x of
  lt@ExprLet{..}
    | null exprDefs -> pure exprBody
    | otherwise -> do
        d <- mapM f expDefs
        let (tail,nonTail) = mconcatMap collectFuncs (body : map (lamExp . funcDefBody) defs)
            notNormal =  nonTail `intersection` fromList (map funcDefName defs)
            myDefs = fromList $ map funcDefName defs
        return $ lt { expDefs = d', 
                      expunCalls = (tail \\ myDefs, nonfail \\ myDefs),
                      expNonNormal = notNormal,
                      expIsNormal = isEmpty notNormal }
  e -> pure e



exprTransformExpr :: (Monad m, Expr Expression'' expr)
                  => (expr -> m expr) -> expr -> m expr
exprTransformExpr f x = exprWrap $ case exprUnwrap x of
  ExprBind e1 l -> ExprBind <$> f a <*> lamTransformBOdy f l
  ExprLet{..} -> exprTransformLet (funcdefTransformLam (lamTransformBody f)) x
  _ -> exprTransformLam (lamTransformBody f)







-- ---------------------------------------------------------------------------


renamePattern :: (Monad m, MonadFresh m)
              => [ExpVal exp]
              -> EMap.EnumMap Var Val
              -> m ([ExpVal exp], EMap.ENumMap Var Val)
renamePattern = go []
  where
    go r [] env = return (r, env)
    go r (x:xs) env =
      case x of
        ValVar v t -> do
          nv <- ValVar <$> fresh <*> pure t
          env' <- EMap.insert nv env
          go (nv:r) xs env'
        ValNodeC t vs -> do
          (vs', env') <- renamePattern vs env
          go (NodeC t vs':r) xs env'
        ValIndex a b -> do
          ([a'], env') <- renamePattern [a] env
          ([b'], env'') <- renamePattern [b] env'
          go (ValIndex a' b':r) xs env''




transformFunction :: (Monad m, MonadFresh m, Expression Expression'' exp)
                  => ([ExpVal exp] -> m ())
                  -> (([ExpVal exp], exp) -> m (ExpVal exp Val, exp))
                  -> (exp -> m exp)
                  -> ExpLambda exp
                  -> m (ExpLambda exp)
transformFunction sub te tf = dc empty
  where
    dc env (Lambda p e) = do
       (p', env') <- renamePattern p env
       sub p'
       (z, _) <- f e [] env'
       return (Lambda p' z)
    --
    f (ExprBind a (Lambda v b)) xs env = f a ((env, v, b):xs) env
    f a@(Return (xs@(_:_:_))) ((senv,p@(ys@(_:_:_)),b):rs) env | length xs == length ys  = do
        Return xs <- mapExpVal (applySubst env) a
        (ys, env') <- renamePattern p
        ts <- mapM te [([y],Return [x]) | x <- xs | y <- ys ]
        z <- f b rs (env' <> senv)
        let h [] = z
            h ((p,v):rs) = v :>>= p :-> h rs
        return $ h [ (p,v) |  Just (p,v) <- ts]
    f a ((senv, p, b):xs) env = do
        a'' <- g env a
        (p'', env') <- renamePattern p
        x <- te (p'', a'')
        z <- f b xs (env' <> senv)
        return $ case x of
            Just (p',a') -> ExprBind a' (Lambda p' z)
            Nothing -> z
    f x [] env = g env x >>= tf
    --
    g env (ExprCase v as) = ExprCase <$> applySubst env v <*> mapM (dc env) as
    g env (ExprGcRoots vs body) = ExprGcRoots <$> mapM (applySubst env) vs <*> f body [] env
    g env lt@ExprLet {..} = do
        body <- f expBody [] env
        defs <- forM expDefs $ \case FuncDef {..} -> createFuncDef True funcDefName <$> dc env funcDefBody
        return $ updateLetProps lt { expBody = body, expDefs = defs }
    g env x = mapExpVal (applySubst env) x



-- ---------------------------------------------------------------------------


sRepeatUnder f xs = any (not . null . tail) $ sortGroupUnder f xs
hasRepeatUnder f xs = any (not . null . tail) $ sortGroupUnder f xs


normalizeGrin :: Grin -> Grin
normalizeGrin grin = setGrinFunctions (f (grinFuncs grin) [] (Right 1)) grin  where
    f [] xs _ = reverse xs
    f ((a,lm):xs) ys set = f xs ((a,lm'):ys) set' where
        (Identity (lm',set')) = fizz  (\_ x -> x) (return . Just) return set lm

normalizeGrin' :: Grin -> Grin
normalizeGrin' grin = setGrinFunctions (f (grinFuncs grin) []) grin  where
    f [] xs  = reverse xs
    f ((a,lm):xs) ys  = f xs ((a,lm'):ys) where
        (Identity (lm',_)) = whiz (\_ x -> x) (return . Just) return (Right 1) lm

whizExps :: Monad m => (Exp -> m Exp) -> Lam -> m Lam
whizExps f l = liftM fst $ whiz (\_ x -> x) (\(p,e) -> f e >>= \e' -> return  (Just (p,e'))) f whizState l




fizz :: Monad m =>
    (forall a . [Val] -> m a -> m a)         -- ^ called for each sub-code block, such as in case statements
    -> (([Val],Exp) -> m (Maybe ([Val],Exp)))  -- ^ routine to transform or omit simple bindings
    -> (Exp -> m Exp)       -- ^ routine to transform final statement in code block
    -> WhizState            -- ^ Initial state
    -> Lam                  -- ^ input lambda expression
    -> m (Lam,WhizState)
fizz sub te tf inState start = res where
    res = runStateT (dc mempty start) inState
    f (a :>>= (v :-> b)) xs env = f a ((env,v,b):xs) env
    f a@(Return (xs@(_:_:_))) ((senv,p@ys,b):rs) env | length xs == length ys  = do
        Return xs <- g env a
        (ys,env') <- renamePattern p
        z <- f b rs (env' `mappend` senv)
        ts <- lift $ mapM te (reverse [([y],Return [x]) | x <- xs | y <- ys ])
        let h [] = z
            h ((p,v):rs) = v :>>= p :-> h rs
        return $ h [ (p,v) |  Just (p,v) <- reverse ts]
    f (Error msg ty) [] env = do
        lift $ tf (Error msg ty)
    f (Error msg ty) ((_,_,b):xs) env = do
        f (Error msg (getType b)) xs env
    f a ((senv,p,b):xs) env = do
        a <- g env a
        (p,env') <- renamePattern p
        z <- f b xs (env' `mappend` senv)
        x <- lift $ te (p,a)
        case x of
            Just (p',a') -> do
                return $ a' :>>= (p' :-> z)
            Nothing -> do
                return z
    f x [] env = do
        x <- g env x
        lift $ tf x
    g env (Case v as) = do
        v <- applySubst env v
        as <- mapM (dc env) as
        return $ Case v as
    g env (GcRoots vs body) = do
        vs <- mapM (applySubst env) vs
        body <- f body [] env
        return $ GcRoots vs body
    g env lt@Let { expDefs = defs, expBody = body } = do
        body <- f body [] env
        let f def@FuncDef { funcDefName = n, funcDefBody = b } = do
                b <- dc env b
                return $ createFuncDef True n b
        defs <- mapM f defs
        return $ updateLetProps lt { expBody = body, expDefs = defs }
    g env x = applySubstE env x
    dc env (p :-> e) = do
        (p,env') <- renamePattern p
        g <- get
        (z,g) <- lift $ sub p $ runStateT  (f e [] (env' `mappend` env)) g
        put g
        return (p :-> z)

applySubstE env x = mapExpVal (applySubst env) x

applySubst env x = f x where
    f var@(Var v _)
        | Just n <- mlookup v env =  return n
    f x = mapValVal f x

newVarName :: MonadState WhizState m => Var -> m Var
newVarName (V sv) = do
    s <- get
    case s of
        Left s -> do
            let nv = v sv
                v n | n `member` s = v (n + size s)
                    | otherwise = n
            put (Left $! insert nv s)
            return (V nv)
        Right n -> do
            put $! (Right $! (n + 1))
            return $ V n
