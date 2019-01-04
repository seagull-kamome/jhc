module Language.Grin.Lint(
  progTypecheck --, dumpProgram
    ) where

import Prelude hiding(fail)

import Data.Maybe (isJust)
import qualified Data.Text as T
import Control.Monad.IO.Class
import Control.Monad.Fail (MonadFail(..))
-- import System.Environment
-- import System.IO (Handle, IOMode(WriteMode), openFile, hClose, hPutStrLn, withFile)

import Control.Monad.Trans.Maybe
import Control.Monad.Reader hiding(fail)

--import qualified Data.EnumSet.EnumSmallBitSet as EBSet -- containers-missing
import qualified Data.EnumSet.EnumSet as ESet          -- containers-missing
--import qualified Data.Set as Set -- containers

import Jhc.Logging

import Language.Grin.AST.Program
import Language.Grin.AST.BasicOperation
import Language.Grin.AST.Lambda
import Language.Grin.AST.Var
import Language.Grin.AST.Val
-- import Language.Grin.AST.Tag
import Language.Grin.AST.Type
import Language.Grin.AST.Expression
import Language.Grin.Internal.Classes
import Language.Grin.Internal.Classes.PrimOpr
import Language.Grin.Data.TypeEnv
import Language.Grin.Data.Config

import Text.PrettyPrint.ANSI.Leijen hiding((<$>))

-- ---------------------------------------------------------------------------

data TcEnv sym primtypes = TcEnv {
    envTyEnv       :: TypeEnv sym primtypes,
    envInScope     :: ESet.EnumSet Var,
    envGrinConfig  :: GrinConfig,
    envLogFunc :: LogFunc
}
instance HasGrinConfig (TcEnv sym primtypes) where grinConfig = envGrinConfig
instance HasLogFunc (TcEnv sym primtypes) where getLogFunc = envLogFunc
instance HasTypeEnv (TcEnv sym primtypes) sym primtypes where
  getTypeEnv = envTyEnv

progTypecheck :: (Monad m, MonadIO m, MonadFail m,
                  MonadReader env m, HasLogFunc env,
                  HasGrinConfig env,
                  Ord sym, Pretty sym,
                  Eq primtypes, Pretty primtypes, PrimType primtypes,
                  Pretty primval,
                  PrimOpr primopr, Pretty primopr,
                  Bounded Var
                 )
              => Program sym primtypes primopr primval -> m ()
progTypecheck Program{..} = do
  env <- ask
  when (isGrinSwitchEnabled CfgLintCheck env) $ do
    bs <- runReaderT (mapM (runMaybeT . lamTypecheck' . funcDefBody) progFunctions)
            TcEnv {
              envTyEnv = progTypeEnv,
              envInScope = ESet.fromList $ map fst progCafs,
              envGrinConfig = grinConfig env,
              envLogFunc = getLogFunc env } -- :: m [Maybe [Typ primtypes]]
    unless (all isJust bs || isGrinSwitchEnabled CfgKeepGoing env) $
      fail "There are type errors."




-- ---------------------------------------------------------------------------
-- Typechecker

logsrc :: T.Text
logsrc = "Grin.Link"



prettyTypemismatch :: (Pretty primtypes)
                   => Doc 
                   -> [Typ primtypes] -> [Typ primtypes]
                   -> Doc -> Doc
prettyTypemismatch somewhere excpt act cnxt =
    "Type mismatch." <> line <> 
      indent 2 ("Expected" <> colon <+> f excpt <> line <>
                "Actual" <> colon <+> f act <> line <>
                "in" <+> somewhere <> colon <+> pretty cnxt)
  where
    f xs = brackets (align $ cat $ punctuate comma $ map pretty xs)







lamTypecheck :: (Monad m, MonadIO m, MonadFail m,
                 Expr expr,
                 sym ~ ExprSym expr, primtypes ~ ExprPrimTypes expr,
                 primval ~ ExprPrimVal expr,
                 Expression'' ~ ExprRep expr,
                 Ord sym, Pretty sym,
                 Eq primtypes, PrimType primtypes, Pretty primtypes,
                 Pretty primval,
                 Pretty expr,
                 Pretty (Lambda expr),
                 MonadReader (TcEnv sym primtypes) m)
             => [Typ primtypes] -> Lambda expr -> m [Typ primtypes]
lamTypecheck ty lam@Lambda{..} =
  local (\e -> e { envInScope = valFreeVars lamBind <> envInScope e }) $ do
    t <- mapM valTypecheck lamBind
    when (ty /= t) $
      logErrorFail logsrc $
        "Type mismatch:" <+> align ("Expected:" <+> pretty ty <> line
                                 <> "Actual:" <+> pretty t <> line)
         <> indent 2 ("in lambda expression" <+> pretty lam)
    exprTypecheck lamExpr



lamTypecheck' :: (Monad m, MonadIO m, MonadFail m,
                  Expr expr, Expression'' ~ ExprRep expr,
                  sym ~ ExprSym expr, primtypes ~ ExprPrimTypes expr,
                  primval ~ ExprPrimVal expr,
                  Ord sym, Pretty sym,
                  Eq primtypes, PrimType primtypes, Pretty primtypes,
                  Pretty primval,
                  Pretty expr,
                  MonadReader (TcEnv sym primtypes) m)
              => Lambda expr -> m [Typ primtypes]
lamTypecheck' Lambda{..} =
  local (\e -> e { envInScope = valFreeVars lamBind <> envInScope e }) $
    mapM_ valTypecheck lamBind >> exprTypecheck lamExpr






valTypecheck :: (Monad m, MonadIO m, MonadFail m,
                 Ord sym,
                 Pretty sym,
                 PrimType primtypes, Eq primtypes,
                 Pretty primval, Pretty primtypes,
                 MonadReader (TcEnv sym primtypes) m)
             => Val sym primtypes primval -> m (Typ primtypes)
valTypecheck = \case
  e@(ValVar v t) -> do
    b <- ESet.member v <$> asks envInScope
    when b $ logErrorFail logsrc $ "Variable not in scope:" <> pretty e
    return t
  ValLit _ t -> return t
  ValUnit    -> return TypUnit
  ValConst t -> \case { TypNode -> TypINode; v-> TypPtr v } <$> valTypecheck t
  ValIndex v offset -> do
    valTypecheck offset >>= \case
      TypPrim _ -> return ()
      x -> logErrorFail logsrc $ "Need primitive type, but" <+> pretty x <> "."
    valTypecheck v
  ValUnknown t -> return t
  ValPrim _ vs ty -> mapM_ valTypecheck vs >> return ty
  n@(ValNodeC tg as) -> do
    TypeOfType { typSlots = as' } <- findTypeOfType tg
    as'' <- mapM valTypecheck as
    if as'' == as'
       then return TypNode
       else logErrorFail logsrc $
         "NodeC:" <+> align ("arguments do not match" <+> pretty as'' <> line
                             <> "with:" <+> pretty as' <> line
                             <> "in context:" <+> pretty n)
  ValItem _ t -> return t




exprTypecheck :: (Monad m, MonadIO m, MonadFail m,
                  Expr expr, Expression'' ~ ExprRep expr, Pretty expr,
                  sym ~ ExprSym expr, primtypes ~ ExprPrimTypes expr,
                  primval ~ ExprPrimVal expr,
                  Ord sym, Pretty sym,
                  PrimType primtypes, Eq primtypes, Pretty primtypes,
                  Pretty (Lambda expr), Pretty primval,
                  MonadReader (TcEnv sym primtypes) m)
              => expr -> m [Typ primtypes]
exprTypecheck e = case exprUnwrap e of
  ExprBind lhs rhs -> do
    ts <- exprTypecheck lhs
    lamTypecheck ts rhs
  ExprPrim _ vs t' -> mapM_ valTypecheck vs >> pure t'
  ExprBaseOp (Apply t) vs -> mapM valTypecheck vs >>= \case
    (v':_) -> if v' == TypNode
       then pure t
       else logErrorFail logsrc $ "App aplly arg doesn't match:" <+> pretty e
    _ -> logErrorFail logsrc "Bad argument of Apply."
  ExprBaseOp Eval v -> mapM valTypecheck v >>= \case
    [TypINode] -> pure [TypNode]
    _ -> logErrorFail logsrc $ "App aval arg doesn't math:" <+> pretty e
  ExprBaseOp (StoreNode _) [v@ValNodeC{}] -> valTypecheck v >> exprType e
  ExprBaseOp Promote vs -> mapM valTypecheck vs >>= \case
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
    TypeOfType { typSlots = as', typReturn = t' } <- findTypeOfType fn
    as'' <- mapM valTypecheck as
    if t' == t then
        if as'' == as'
           then return t'
           else logErrorFail logsrc $
             prettyTypemismatch "APP argument" as' as'' (pretty e)
     else logErrorFail logsrc $
             prettyTypemismatch "App results" t t' (pretty e)
  ExprAlloc v c r -> valTypecheck c >> valTypecheck r >> valTypecheck v
                       >>= \t -> pure [TypPtr t]
  ExprReturn vs -> mapM valTypecheck vs
  ExprError _ t -> pure t
  ExprCase _ [] -> logErrorFail logsrc "Empty case."
  ExprCase v as -> do
    tv <- valTypecheck v
    es <- mapM (lamTypecheck [tv]) as
    let g (x0:x1:xs) (y0:y1:ys) = do
            when (x0 /= x1) $
              logErrorFail logsrc $ prettyTypemismatch "case clause" x0 x1 (pretty y1)
            g (x0:xs) (y0:ys)
        g _ _ = return True
    _ <- g es as
    return $ head es
  ExprLet{..} -> do
    te <- asks getTypeEnv
    case extendTyEnv expDefs te of
      Left err -> logErrorFail logsrc $
        "failed to extend TypeEnv" <> hsep (map pretty err)
      Right ty ->
        local (\env' -> env' { envTyEnv = ty }) $ do
          mapM_ (lamTypecheck' . funcDefBody) expDefs
          exprTypecheck expBody
  _-> logErrorFail logsrc $ "Bad expr passed to exprTypecheck" <> pretty e




-- ---------------------------------------------------------------------------
-- Dump


escapeStr :: String -> String
escapeStr = \case
  "" -> ""
  '\'':xs -> "''" ++ escapeStr xs
  x:xs -> x:escapeStr xs

dshow :: Pretty a => a -> String
dshow x = escapeStr $ displayS (renderCompact $ plain $ pretty x) ""


funArg, funRet :: Pretty a => a -> Int -> String
funArg n i = dshow n ++ "@arg@" ++ show i
funRet n i = dshow n ++ "@ret@" ++ show i

#if 0
dumpProgram :: (Monad m, MonadIO m, MonadReader env m,
                HasGrinConfig env,
                Expr expr, Expression'' ~ ExprRep expr,
                sym ~ ExprSym expr, primtypes ~ ExprPrimTypes expr,
                primopr ~ ExprPrimOpr expr, primval ~ ExprPrimVal expr,
                Pretty sym, Pretty primtypes, Pretty primopr, Pretty primval)
            => String
            -> String
            -> Program sym primtypes primopr primval -> m ()
dumpProgram filename phase prg@Program{..} = do
  cfg <- asks grinConfig
  liftIO $ do
    args <- getArgs
    withFile (filename ++ "_" ++ phase ++ ".grin") WriteMode $ \h ->
      hPutDoc h $
        "--" <+> text (unwords args) <> line <>
        "--" <+> grinVersion <> line <>
        pretty prg
    when (isGrinSwitchEnabled CfgDumpDatalog cfg) $
      withFile (filename ++ "_" ++ phase ++ ".datalog") WriteMode $ \h -> do
        hPutDoc h $
          "%" <+> text (unwords args) <> line <>
          "%" <+> grinVersion <> line
        hPrintGrinDL h
    when (isGrinSwitchEnabled CfgDumpGrin cfg) $
      putDoc $ align $ "v--" <+> text phase <+> "Grin" <> line <>
                        pretty prg <> line <>
                        "^--" <+> text phase <+> "Grin" <> line
  where
    hPrintGrinDL h = do
      unless (null progCafs) $ do
        hPutStrLn h "% cafs"
        forM_ progCafs (\(x,y) -> hPutStrLn h $
          "what(" ++ dshow x ++ ",'caf').\n"
           ++ "typeof(" ++ dshow y ++ ",inode)." )
      hPutStrLn h "% functions"
      hPutDoc h $ vsep $ map pretty progFunctions
    --
    printFunc h n Lambda{..} = do
      hPutStrLn h $ "func(" ++ dshow n ++"," ++ show (length lamBind) ++ "),"
      hPutStrLn h $ unlines $ flip concatMap (zip [0..] lamBind) $ \(i, ValVar v t) ->
        let x = funArg n i
         in ["perfom(assign," ++ dshow v ++ "," ++ x ++ ").",
             "whar(" ++ x ++ ",funarg).",
             "typeof(" ++ x ++ dshow t ++ ").",
             "typeof(" ++ dshow v ++ "," ++ dshow t ++ ")." ]
      rts <- exprType lamExpr
      let lts = [ (t, funRet n i) | t <- rts | i <- [0..] ]
      hPutStrLn h $ unlines $
        map (\x -> "what(" ++ snd x ++ ",funret).") lts
        ++ map (\(t, n) -> "typeof(" ++ n ++ "," ++ dshow t ++ ").") lts
      printDL h n (map (Left . snd) lts) lamExpr





printDL :: (Monad m, MonadIO m,
            Expr expr, Expression'' ~ ExprRep expr,
            sym ~ ExprSym expr, primtypes ~ ExprPrimTypes expr,
            primval ~ ExprPrimVal expr)
        => Handle
        -> Tag sym
        -> [Either String (Val sym primtypes primval)]
        -> expr
        -> m ()
printDL h n bs e = liftIO $ f' bs e where
  f' bs e = f bs $ exprUnwrap e
  --
  f bs (ExprBind x Lambda{..}) = f (map Right lamBind) x >> f' bs lamExpr
  f bs ExprReturn{..} = zipWithM_ (assign "assign") bs expValues
  f [b] (ExprBaseOp Eval [x]) = assign "eval" b x
  f bs (ExprApp fn as ty) = do
     forM_ (zip [0..] as) $ \(i, a) -> assign "assign" (Left $ funArg fn i) a
     forM_ (zip [0..] bs) $ \(i, a) -> genAssign "assign" a (Left $ funRet fn i)
  f bs (ExprCase v ls) = mapM_ (f' bs . ExprBind (ExprReturn [v])) ls
  f bs ExprLet{..} = do
      forM_ expDefs $ hPutDoc h . pretty
      forM_ expDefs $ \FuncDef{..} -> hPutStrLn h $ "subfunc(" ++ dshow funcDefName + "," ++ dshow n ++ ")."
      f' bs expBody
  f _ ExprError {} = return ()
  f bs ExprCall { expValue = ValItem fn _, expArgs =  as, expType = ty} = do
      forM_ (zip [0..] as) $ \ (i,a) -> assign "assign" (Left $ funArg fn i) a
      forM_ (zip [0..] bs) $ \ (i,a) -> genAssign "assign" a (Left $ funRet fn i)
  f bs e = zipWithM_ (assign "assign") bs (map ValUnknown (exprType e))
  --
  assign op b v = genAssign op b (Right v)
  --
  g op x y = hPutStrLn h $ concat ["perform(", op, ",", x, "," y, ")." ]
  genAssign op (Left b) (Left l) = g op b l
  genAssign op (Left b) (Right (ValVar v _)) = g op b (dshow v)
  genAssign op (Left b) (Right ValConst{}) = g b "const"
  genAssign op (Left b) (Right v) = do
    ty <- valType v
    when (ty == TypNode || ty == TypINode) $ setUnknown h b (show (op,v))
  genAssign op (Right (ValVar v1 _)) (Left l) = g op (dshow v1) l
  genAssign op (Right (ValVar v1 _)) (Right (ValVar v2 _)) = g op (dshow v1) (dshow v2)
  genAssign op (Right b) rv =  bindUnknown h b (take 20 $ show (op,rv))
  --
  setUnknown h x r = hPutStrLn h $ "unknown(" ++ dshow x ++ "," ++ dshow r ++ ")."
  --
  bindUnknown h l r =
    mapM_ (\ (x,t) ->
             when (t == TypNode || t == TypINode) $ setUnknown h x r)
          (Set.toList $ valFreeVars' l)
#endif

