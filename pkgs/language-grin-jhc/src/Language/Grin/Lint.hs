module Language.Grin.Lint(
  progTypecheck, dumpProgram
    ) where

import qualified Data.Text as T
--import Control.Monad.Fail
import Control.Monad.Reader
import Control.Exception
import System.Environment
import System.IO (Handle, IOMode(WriteMode), openFile, hClose)

import qualified Data.EnumSet.EnumSmallBitSet as EBSet -- containers-missing
import qualified Data.EnumSet.EnumSet as ESet          -- containers-missing

import Jhc.Logging

import Language.Grin.AST.Program
import Language.Grin.AST.BasicOperation
import Language.Grin.AST.Lambda
import Language.Grin.AST.Var
import Language.Grin.AST.Val
import Language.Grin.AST.Tag
import Language.Grin.AST.Type
import Language.Grin.AST.Expression
import Language.Grin.Internal.Classes
import Language.Grin.Data.TypeEnv
import Language.Grin.Data.Config

import Text.PrettyPrint.ANSI.Leijen hiding((<$>))

-- ---------------------------------------------------------------------------


progTypecheck :: (Monad m, MonadIO m, MonadReader env m, HasLogFunc env,
                 HasGrinConfig env)
              => Program -> m ()
progTypecheck Program{..} = do
  env <- asks grinConfig
  when (EBSet.member CfgLitCheck $ cfgSwitches $ grinConfig env) $ do
    b <- lefts <$> runReader (map (runExceptT lamTypecheck') $ funcDefBody progFunctions)
                             TcEnv {
                               envTyEnv = progTypeEnv,
                               envInScope = fromList $ map fst progCafs,
                               envGrinConfig = grinConfig env,
                               envLogFunc = getLogFunc env }
    unless (null b || EBSet.member CfgKeepGoing (cfgSwitches $ grinConfig env))
      fail "There are type errors."




-- ---------------------------------------------------------------------------
-- Typechecker

logsrc :: T.Text
logsrc = "Grin.Link"

data TcEnv sym primtypes = TcEnv {
    envTyEnv       :: TypeEnv sym primtypes,
    envInScope     :: ESet.EnumSet Var,
    envGrinConfig  :: GrinConfig,
    envLogFunc :: LogFunc
}
instance HasGrinConfig TcEnv where grinConfig = envGrinConfig





lamTypecheck :: (Monad m, MonadIO m, MonadReader TcEnv m,
                 primtypes ~ ExprPrimTypes expr)
             => [Typ primtypes] -> Lambda expr -> m [Typ primtypes]
lamTypecheck ty lam@Lambda{..} =
  local (\e -> e { envInScope = valFreeVars lamBind <> envInScope e }) $
    mapM tcVal v
      >>= \t -> do
             when (ty /= t) $
               logErrorFail logsrc $
                 "Type mismatch:" <+> align ("Expected:" <+> pretty ty <> line
                                             "Actual:" <+> pretty t <> line)
                  <> indent 2 ("in lambda expression" <+> pretty lam)
             return ty
      >>= exprTypecheck lamExpr



lamTypecheck' :: (Monad m, MonadIO m, MonadReader TcEnv m,
                  primtypes ~ ExprPrimTypes expr)
              => Maybe [Typ primtypes] -> Lambda expr -> m [Typ primtypes]
lamTypecheck' lam@Lambda{..} =
  local (\e -> e { envInScope = valFreeVars lamBind <> envInScope e }) $
    mapM tcVal v >>= exprTypecheck lamExpr



valTypecheck :: (Monad m, MonadIO m, MonadReader TcEnv m, MonadFail m,
                 sym ~ ExprSym expr, primtypes ~ ExpPrimTypes expr,
                 primval ~ ExprPrimVal expr)
             => Val sym primtypes primval -> m (Typ primtypes)
valTypecheck = \case
  e@(ValVar v t) -> do
    asks envInScope >>= ESet.member v >>= when $
      logError logsrc $ "Variable not in scope:" <> pretty e
      fail "type error."
    return t
  ValLit _ t -> return t
  ValUnit    -> return TypUnit
  ValConst t -> \case { TypNode -> TypINode; v-> TypPtr v } <$> valTypecheck t
  ValIndex v offset -> do
    valTypecheck offset >>= \case
      TypPrim -> return ()
      x -> logErrorFail logsrc $ "Need primitive type, but" <+> pretty x <> "."
    valTypecheck v
  ValUnknown t -> return t
  ValPrim _ vs ty -> mapM_ valTypecheck vs >> return ty
  n@(ValNodeC tg as) -> do
    te <- asks envTyEnv
    (as',_) <- findArgsType te tg
    as'' <- mapM valTypecheck as
    if as'' == as'
       then return TyNode
       else logErrorFail logsrc $
         "NodeC:" <+> align ("arguments do not match" <+> pretty as'' <> line
                             <> "with:" <+> pretty as' <> line
                             <> "in context:" <+> pretty n)
  ValItem _ t -> return t




exprTypecheck :: (Monad m, MonadIO m, MonadReader TcEnv m, MonadFail m,
                  Expr Expression'' expr, Pretty expr
                  primtypes ~ ExprPrimTypes expr)
              => expr -> m [Typ primtypes]
exprTypecheck e = case exprUnpack e of
  ExprBind lhs rhs -> lamTypecheck <$> exprTypecheck lhs <*> pure rhs
  ExprPrim p as t' -> mapM_ valTypecheck vs >> pure t'
  ExprBaseOp (Apply t) vs -> mapM valTypecheck vs >>= \case
    (v':_) -> if v' == TypNode
       then pure t
       else logErrorFail logsrc $ "App aplly arg doesn't match:" <+> pretty e
    _ -> logErrorFail logsrc "Bad argument of Apply."
  ExprBaseOp Eval v -> mapM valTypecheck v >>= \case
    [TypINode] -> pure [TyNode]
    _ -> logErrorFail logsrc $ "App aval arg doesn't math:" <+> pretty e
  ExprBaseOp (StoreNode _) [v@ValNodeC{}] -> valTypecheck v >> exprType e
  ExprBaseOp Promote vs -> mapM valTypcheck vs >>= \case
    [TypINode] -> pure [TypNode]
    _ -> logErrorFail logsrc "Bad arguments of Promote."
  ExprBaseOp Demote vs -> mapM valTypecheck vs >>= \case
    [TypNode] -> pure [TypINode]
    _ -> logErrorFail logsrc "Bad arcuments of Demote."
  ExprBaseOp Overwrite [w, v@ValNodeC{}] ->
    valTypecheck w >> valTypecheck v >> pure []
  ExprBaseOp PokeVal vs -> mapM valTypecheck vs >>= \case
    [TypPtr t, ty] ->
      if t == ty then pure []
                 else logErrorFail logsrc "PokeVal: type doesn't match."
    _ -> logErrorFail logsrc "Bad arguments of PokeVal."
  ExprBaseOp PeekVal vs -> mapM valTypecheck vs >>= \case
    [TypPtr t] -> pure [t]
    _ -> logErrorFail logsrc "Bad argumebts of PeekVal."
  ExprApp fn as t -> do
    te <- asks envTyEnv
    (as',t') <- findArgsType te fn
    as'' <- mapM tcVal as
    if t' == t then
        if as'' == as'
           then return t'
           else logErrorFail logsrc $ "App: arguments do not match: " ++ show (a,as',t')
     else logErrorFail logsrc $ "App: results do not match: " ++ show (a,t,(as',t'))
  ExprAlloc v c r -> valTypecheck c >> valTypecheck r >> valTypecheck v
                       >>= \t -> pure [TyPtr t]
  ExprReturn vs -> mapM valTypecheck vs
  ExprError _ t -> pure t
  ExprCase _ [] -> logErrorFail logsrc "Empty case."
  ExprCase v as -> do
    tv <- valTypecheck v
    mapM_ (lamTypecheck tv) as
  ExprLet{..} ->
    local (\e -> e { envTyEnv = extendTyEnv exprDefs (envTyEnv e)}) $
      mapM_ (lamTypecheck' . funcDefBody) exprDefs
      exprTypecheck exprBody
  _-> logErrorFail logsrc $ "Bad expr passed to exprTypecheck" <> pretty e




-- ---------------------------------------------------------------------------
-- Dump


dumpProgram :: (Monad m, MonadIO m, MonadReader env m,
                HasGrinConfig m,
                Pretty (Program sym primtypes primopr primval),
                Pretty (Val sym primtypes primval),
                Pretty (Tag sym) )
            => String
            -> String
            -> Program sym primtypes primopr primval -> m ()
dumpProgram filename phase prg@Program{..} = do
  cfg <- asks grinConfig

  args <- lift getArgs
  bracket (openfile (filename ++ "_" ++ phase ++ ".grin") WriteMode)
          hClose $ \h ->
    hPutDoc h $
      "--" <+> text (unwords args) <> line <>
      "--" <+> grinVersion <> line <>
      pretty prg
  when (EBSet.member CfgDumpDatalog (cfgSwitches cfg))
    bracket (openFile (filename ++ "_" ++ phase ++ ".datalog") WriteMode)
            hClose $ \h -> do
      hPutDoc h $
        "%" <+> text (unwords args) <> line <>
        "%" <+> grinVersion <> line
      hPrintGrinDL h prg
  when (EBSet.member CfgDumpGrin (cfgSwitches cfg))
    putDoc "v--" <+> phase <+> "Grin" <> line
           pretty prg <> line <>
           "^--" <+> phase <+> "Grin" <> line
  where
    hPrintGrinDL h = do
      unless (null progCafs) $ do
        hPutStrLn h "% cafs"
        forM_ progCafs (\(x,y) -> do
                  hPutStrLn h "what(" ++ dshow x ++ ",'caf')."
                  hPutStrLn h "typeof(" ++ dshow y ++ ",inode)." )
      hPutStrLn h "% functions"
      forM_ progFunctions $ \FuncDef{..} -> printFunc h funcDefName funcDefBody
    --
    printFunc h n Lambda{..} = do
      hPutStrLn h $ "func(" ++ dshow n ++"," ++ show (length lamBind) ++ "),"
      hPutStrLn h $ unlines $ for (zip [0..] l) $ \(i, ValVar v t) ->
        let x = dshow n ++ "@arg@" ++ show i
         in ["perfom(assign," ++ dshow v ++ "," ++ x ++ ").",
             "whar(" ++ x ++ ",funarg).",
             "typeof(" ++ x ++ dshow t ++ ").",
             "typeof(" ++ dshow v ++ "," ++ dshow t ++ ")." ]
      rts <- exprType lamExpr
      let lts = [ (t, dshow n ++ "@ret@" ++ show i) | t <- rts | i <- [0..] ]
      hPutStrLn h $ unlines $
        map (\x -> "what(" ++ snd x ++ ",funret).") lts
        ++ map (\(t, n) -> "typeof(" ++ n ++ "," ++ dshow t ++ ").") lts
      printDL h n (map (Left . snd) lts) lamExpr





escapeStr :: String -> String
escapeStr = \case
  "" -> ""
  '\'':xs -> "''" ++ escapeStr xs
  x:xs -> x:escapeStr xs

dshow :: Pretty a => a -> String
dshow = escapeStr . plain . pretty




funArg n i = show n ++ "@arg@" ++ show i
funRet n i = show n ++ "@ret@" ++ show i

bindUnknown h l r = do
    mapM_ (\ (x,t) -> when (tyInteresting t) $ setUnknown h x r) (Set.toList $ freeVars l :: [(Var,Ty)])

setUnknown :: DShow a => Handle -> a -> String -> IO ()
setUnknown h x r = do hPrintf h "unknown(%s,%s).\n" (dshow x) (dshow r)

printDL h n fs e = f fs e where
    f fs (x :>>= l :-> y) = do
        f (map Right l) x
        f fs y
    f bs (Return vs) = do zipWithM_ (assign "assign") bs vs
    f [b] (BaseOp Eval [x]) = do assign "eval" b x
    f b (App fn as ty) = do
        forM_ (zip naturals as) $ \ (i,a) -> do
            assign "assign" (Left $ funArg fn i) a
        forM_ (zip naturals b) $ \ (i,a) -> do
            genAssign "assign" a (Left $ funRet fn i)
    f b (Case v ls) = mapM_ (\l -> f b (Return [v] :>>= l)) ls
    f b Let { expDefs = defs, expBody = body } = do
        forM_ defs $ \d -> printFunc h (funcDefName d) (funcDefBody d)
        forM_ defs $ \d -> hPrintf h "subfunc(%s,%s).\n" (dshow $ funcDefName d) (dshow n)
        f b body
    f b Error {} = return ()
    f b Call { expValue = Item fn _, expArgs =  as, expType = ty} = do
        forM_ (zip naturals as) $ \ (i,a) -> do
            assign "assign" (Left $ funArg fn i) a
        forM_ (zip naturals b) $ \ (i,a) -> do
            genAssign "assign" a (Left $ funRet fn i)

    f bs e = do zipWithM_ (assign "assign") bs (map ValUnknown (getType e))
    assign op b v = genAssign op b (Right v)

    genAssign :: String -> Either String Val -> Either String Val -> IO ()
    genAssign op (Left b) (Left l) = hPrintf h "perform(%s,%s,%s).\n" op (dshow b) (dshow l)
    genAssign op (Right (Var v1 _)) (Left l) = hPrintf h "perform(%s,%s,%s).\n" op (dshow v1) (dshow l)
    genAssign op (Left b) (Right (Var v _)) = hPrintf h "perform(%s,%s,%s).\n" op (dshow b) (dshow v)
    genAssign op (Left b) (Right (Const {})) = hPrintf h "perform(%s,%s,%s).\n" op (dshow b) "const"
    genAssign op (Right (Var v1 _)) (Right (Var v2 _)) = hPrintf h "perform(%s,%s,%s).\n" op (dshow v1) (dshow v2)
    genAssign op (Left b) (Right v) = when (tyInteresting $ getType v) $ setUnknown h b (show (op,v))
    genAssign op (Right b) rv =  bindUnknown h b (take 20 $ show (op,rv))

tyInteresting ty = ty == TyNode || ty == tyINode


