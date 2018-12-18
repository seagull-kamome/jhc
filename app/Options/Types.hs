module Options.Types (
    StopCondition(..), Opt(..),
    Mode(..),
    helpUsage,
    postProcessFD
    ,postProcessFO
    ,fileOptions) where

import qualified Data.Text as T
import Data.List(intercalate)
import System.Console.GetOpt

import Options.Map
import qualified Data.Map as M
import qualified Data.Set as S
import qualified FlagDump as FD
import qualified FlagOpts as FO


-- ---------------------------------------------------------------------------



data Mode
    = BuildHl FilePath         -- ^ Build the specified hl-file given a description file.
    | Interactive              -- ^ Run interactively.
    | Version                  -- ^ Print version and die.
    | ShowHelp                 -- ^ Show help message and die.
    | ShowConfig               -- ^ Show configuration info.
    | CompileExe               -- ^ Compile executable
    | ShowHo String            -- ^ Show ho-file.
    | ListLibraries            -- ^ List libraries
    | PrintHscOptions          -- ^ Print options for hsc2hs
    | PurgeCache               -- ^ Purge the cache
    | Preprocess               -- ^ Filter through preprocessor
    deriving(Eq)



data StopCondition
    = StopParse                -- ^ Just parse and rename modules then exit
    | StopTypeCheck            -- ^ Stop after type checking
    | StopC                    -- ^ Stop after producing C code.
    | CompileHo                -- ^ Compile ho
    | StopNot                  -- ^ Don't stop believing.
    deriving(Eq)



data Opt = Opt {
    optMode        :: !Mode,      -- ^ Mode of interaction
    optColumns     :: !Int,       -- ^ Width of terminal.
    optDump        ::  [String],  -- ^ Dump options (raw).
    optStmts       ::  [String],  -- ^ statements to execute
    optFOpts       ::  [String],  -- ^ Flag options (raw).
    optIncdirs     ::  [String],  -- ^ Include directories.
    optCCargs      ::  [String],  -- ^ Optional arguments to the C compiler.
    optHls         ::  [String],  -- ^ Load the specified hl-files (haskell libraries).
    optAutoLoads   ::  [String],  -- ^ AutoLoaded haskell libraries.
    optHlPath      ::  [String],  -- ^ Path to look for libraries.
    optIncs        ::  [String],
    optWith        ::  [String],
    optDefs        ::  [String],
    optExtensions  ::  [String],
    optStop        :: !StopCondition,
    optWorkDir     ::  Maybe FilePath,
    optAnnotate    ::  Maybe FilePath,
    optDeps        ::  Maybe FilePath,
    optHoDir       ::  Maybe FilePath,
    optHoCache     ::  Maybe FilePath,
    optArgs        ::  [String],
    optStale       ::  [String],  -- ^ treat these modules as stale
    optKeepGoing   :: !Bool,      -- ^ Keep going when encountering errors.
    optMainFunc    ::  Maybe (Bool,String),    -- ^ Entry point name for the main function.
    optArch        ::  [String],           -- ^ target architecture
    optCross       :: !Bool,
    optOutName     ::  Maybe String,           -- ^ Name of output file.
    optIgnoreHo    :: !Bool,                   -- ^ Ignore ho-files.
    optNoWriteHo   :: !Bool,                   -- ^ Don't write ho-files.
    optNoAuto      :: !Bool,                   -- ^ Don't autoload packages
    optVerbose     :: !Int,                    -- ^ Verbosity
    optStatLevel   :: !Int,                    -- ^ Level to print statistics
    optInis        ::  M.Map String String,    -- ^ options read from ini files
    optDumpSet     ::  S.Set FD.Flag,    -- ^ Dump flags.
    optFOptsSet    ::  S.Set FO.Flag     -- ^ Flag options (-f\<opt\>).
  }


defaultOptions :: Opt
defaultOptions = Opt {
    optMode        = CompileExe,
    optColumns     = 80,
    optCross       = False,
    optIncdirs     = [],
    optAnnotate    = Nothing,
    optDeps        = Nothing,
    optHls         = [],
    optAutoLoads   = [],
    optHlPath      = [],
    optIncs        = [],
    optWith        = [],
    optDefs        = [],
    optExtensions  = [],
    optStop        = StopNot,
    optDump        = [],
    optStale       = [],
    optStmts       = [],
    optFOpts       = ["default"],
    optCCargs      = [],
    optWorkDir     = Nothing,
    optHoDir       = Nothing,
    optHoCache     = Nothing,
    optArgs        = [],
    optIgnoreHo    = False,
    optNoWriteHo   = False,
    optKeepGoing   = False,
    optMainFunc    = Nothing,
    optArch        = ["default"],
    optOutName     = Nothing,
    optInis        = M.empty,
    optVerbose     = 0,
    optStatLevel   = 1,
    optNoAuto      = False,
    optDumpSet     = S.singleton FD.Progress,
    optFOptsSet    = languageDefault
}


{-# NOINLINE fileOptions #-}
fileOptions :: Monad m => Opt -> [String] -> m Opt
fileOptions options xs = case getOpt Permute theoptions xs of
    (os,[],[]) -> postProcessFD (foldl (flip ($)) options os) >>= postProcessFO
    (_,as,errs) -> fail (unlines $ map f as ++ errs) where
        f x = "fileOptions: Unexpected argument " ++ show x



postProcessFD :: Monad m => Opt -> m Opt
postProcessFD o = case FD.process (optDumpSet o) (optDump o ++ vv) of
        (s,[]) -> return $ o { optDumpSet = s, optDump = [] }
        (_,xs) -> fail ("Unrecognized dump flag passed to '-d': "
                        ++ unwords xs ++ "\nValid dump flags:\n\n" ++ FD.helpMsg)
    where
    vv | optVerbose o >= 2 = ["veryverbose"]
       | optVerbose o >= 1 = ["verbose"]
       | otherwise = []

postProcessFO :: Monad m => Opt -> m Opt
postProcessFO o = case FO.process (optFOptsSet o) (optFOpts o) of
        (s,[]) -> return $ o { optFOptsSet = s, optFOpts = [] }
        (_,xs) -> fail ("Unrecognized flag passed to '-f': "
                        ++ unwords xs ++ "\nValid flags:\n\n" ++ FO.helpMsg)

#if 0
prettyOptions :: String
prettyOptions =  showSD $ vcat ([h1 "Usage Examples"] ++ usages
        ++ [h1 "Option Flags"] ++ flags) where
    colnums = [c1,c2, 80 - c1 - c2 - 2] where
        c1 = ml 0 optList
        c2 = ml 1 optList
        ml n = maximum . map (length . (!! n))
    h1 t = text "# " <> text t $$ text ""
    flags = dashes $+$ header $+$ hdash $+$ vsep (map pads optList) $+$ dashes
    optList :: [[String]]
    optList = map f theoptions
    cmd =  text "    ./jhc [OPTION]"
    dashes = text $ replicate (sum colnums + length colnums) '-'
    usages =
        [cmd <+> text "File.hs   # compile given module"
        ,cmd <+> text "--build-hl libdef.yaml   # compile library described by file"]
    f (Option cs vs arg msg) = [unwords (sflg ++ mflg),rg,msg] where
        sflg = map (\c -> ['-',c]) cs
        mflg =  map (\s -> "--" ++ s) vs
        rg = rarg arg
    rarg NoArg {} = ""
    rarg (ReqArg _ n) = n
    vsep xs = vcat $ intercalate (text "") xs
    hdash = hsep [ text $ replicate n '-' | n <- colnums ]

    header = pads ["flag","arg","description"]
    pads xs = hsep [ pad p x | (x,p) <- zip xs colnums ]
    pad :: Int -> String -> String
    pad n t = t ++ replicate (n - length t) ' '
    len sh = length $ show sh
#endif


