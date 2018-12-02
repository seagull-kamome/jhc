module Options.Type(
    StopCondition(..),
    theoptions,Opt(..),
    Mode(..),
    helpUsage,
    -- optFOptsSet_u,
    -- prettyOptions,
    -- emptyOpt,
    postProcessFD
    ,postProcessFO
    ,fileOptions) where

import Data.List(intercalate)
import System.Console.GetOpt

import Data.Default

import Options.Map
-- import Util.DocLike
import qualified Data.Map as M
import qualified Data.Set as S
import qualified FlagDump as FD
import qualified FlagOpts as FO



{-# NOINLINE theoptions #-}
theoptions :: [OptDescr (Opt -> Opt)]
theoptions =
    [Option ['V'] ["version"]         (NoArg  (\x -> x { optMode = Version }))          "print version info and exit"
    ,Option ['h'] ["help"]            (NoArg  (\x -> x { optMode = ShowHelp }))         "print help information and exit"
    ,Option []    ["info"]            (NoArg  (\x -> x { optMode = ShowConfig }))       "show compiler configuration information and exit"
    ,Option []    ["purge-cache"]     (NoArg  (\x -> x { optMode = PurgeCache }))       "clean out jhc compilation cache"
    ,Option ['v'] ["verbose"]         (NoArg  (\x -> x { optVerbose = optVerbose x + 1}))          "chatty output on stderr"
    ,Option ['z'] []                  (NoArg  (\x -> x { optStatLevel = optStatLevel x + 1 }))        "Increase verbosity of statistics"
    ,Option ['d'] []                  (ReqArg (\y x -> x { optDump = y:optDump x }) "[no-]flag") "dump specified data during compilation"
    ,Option ['f'] []                  (ReqArg (\y x -> x { optFOpts = y:optFOpts x }) "[no-]flag") "set or clear compilation options"
    ,Option ['X'] []                  (ReqArg (\y x -> x { optExtensions = y:optExtensions x })  "Extension") "enable the given language extension by the cabal name. see -fhelp for jhc native names."
    ,Option ['o'] ["output"]          (ReqArg (\y x -> x { optOutName = Just y })  "FILE") "output to FILE"
    ,Option ['i'] ["include"]         (ReqArg (\y x -> x { optIncdirs = idu y $ optIncdirs x }) "DIR")   "path to look for haskell source files. use -i- to clear the search path."
    ,Option ['I'] []                  (ReqArg (\y x -> x { optIncs = idu y $ optIncs x }) "DIR")       "add to preprocessor include path"
    ,Option []    ["with"]            (ReqArg (\y x -> x { optWith = idu y $ optWith x }) "foo.yaml") "include values from yaml file in configuration"
    ,Option ['D'] []                  (ReqArg (\y x -> x { optDefs = y:optDefs x }) "NAME=VALUE") "add new definitions to set in preprocessor"
    ,Option []    ["optc"]            (ReqArg (\y x -> x { optCCargs = idu y $ optCCargs x }) "option") "extra options to pass to c compiler"
    ,Option ['c'] []                  (NoArg  (\x -> x { optStop = CompileHo }))        "just compile the modules, caching the results."
    ,Option ['C'] []                  (NoArg  (\x -> x { optStop = StopC }))            "compile to C code"
    ,Option ['E'] []                  (NoArg  (\x -> x { optMode = Preprocess }))       "preprocess the input and print result to stdout"
    ,Option ['k'] ["keepgoing"]       (NoArg  (\x -> x { optKeepGoing = True }))        "keep going on errors"
    ,Option []    ["cross"]           (NoArg  (\x -> x { optCross =  True }))            "enable cross-compilation, choose target with the -m flag"
    ,Option []    ["stop"]            (ReqArg (\y x -> x { optStop = stop y }) "parse/typecheck/c") "stop after the given pass, parse/typecheck/c"
    ,Option []    ["width"]           (ReqArg (\y x -> x { optColumns = read y})  "COLUMNS") "width of screen for debugging output"
    ,Option []    ["main"]            (ReqArg (\y x -> x { optMainFunc = Just (False, y) }) "Main.main")  "main entry point"
    ,Option ['m'] ["arch"]            (ReqArg (\y x -> x { optArch = idu y $ optArch x }) "arch")      "target architecture options"
    ,Option []    ["entry"]           (ReqArg (\y x -> x { optMainFunc = Just (True, y) })  "<expr>")  "main entry point, showable expression"
--  ,Option ['e'] []                  (ReqArg (\d -> optStmts_u ( d:)) "<statement>")  "run given statement as if on jhci prompt"
    ,Option []    ["show-ho"]         (ReqArg (\y x -> x { optMode = ShowHo y }) "file.ho") "Show contents of ho or hl file. Use -dflag to determine what information to show."
    ,Option []    ["noauto"]          (NoArg  (\x -> x { optNoAuto =  True }))           "Do not automatically depend on primitive libraries. Used to build jhc-prim. Use -p- to clear the visible package list."
    ,Option ['p'] ["package"]         (ReqArg (\y x -> x { optHls = y:optHls x }) "package")   "Load given haskell library package. Use -p- to remove all current entries from the list."
    ,Option ['L'] []                  (ReqArg (\y x -> x { optHlPath = idu y $ optHlPath x }) "path")   "Look for haskell libraries in the given directory. Use -L- to clear the search path."
    ,Option []    ["build-hl"]        (ReqArg (\y x -> x { optMode = BuildHl y }) "desc.yaml") "Build hakell library from given library description file"
    ,Option []    ["annotate-source"] (ReqArg (\y x -> x { optAnnotate = Just y }) "<dir>") "Write preprocessed and annotated source code to the directory specified"
    ,Option []    ["deps"]            (ReqArg (\y x -> x { optDeps = Just y }) "<file.yaml>") "Write dependency information to file specified"
    ,Option []    ["interactive"]     (NoArg  (\x -> x { optMode = Interactive }))      "run interactivly ( for debugging only)"
    ,Option []    ["ignore-cache"]    (NoArg  (\x -> x { optIgnoreHo = True }))         "Ignore existing compilation cache entries."
    ,Option []    ["readonly-cache"]  (NoArg  (\x -> x { optNoWriteHo = True }))        "Do not write new information to the compilation cache."
    ,Option []    ["no-cache"]        (NoArg  (\x -> x { optNoWriteHo = True, optIgnoreHo = True })) "Do not use or update the cache."
    ,Option []    ["cache-dir"]       (ReqArg (\y x -> x { optHoCache = Just y }) "JHC_CACHE")  "Use a global cache located in the directory passed as an argument."
    ,Option []    ["stale"]           (ReqArg (\y x  -> x { optStale = idu y $ optStale x }) "Module")  "Treat these modules as stale, even if they exist in the cache."
    ,Option []    ["list-libraries"]  (NoArg  (\x -> x { optMode = ListLibraries }))    "List of installed libraries."
    ,Option []    ["tdir"]            (ReqArg (\y x -> x { optWorkDir = Just y }) "dir/") "specify the directory where all intermediate files/dumps will be placed."
--   , Option []    ["print-hsc-options"] (NoArg (optMode_s PrintHscOptions)) "print options to pass to hsc2hs"
    ]
  where
    idu :: String -> [String] -> [String]
    idu "-" _ = []
    idu d ds = ds ++ [d]
    stop :: String ->  StopCondition
    stop "parse" = StopParse
    stop "deps" = StopParse
    stop "typecheck" = StopTypeCheck
    stop "c" = StopC
    stop s = StopError s




data Mode
    = BuildHl FilePath         -- ^ Build the specified hl-file given a description file.
    | Interactive              -- ^ Run interactively.
    | Version                  -- ^ Print version and die.
--    | VersionCtx               -- ^ Print version context and die.
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
    = StopError String         -- ^ error
    | StopParse                -- ^ Just parse and rename modules then exit
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

instance Default Opt where
  def = Opt {
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

helpUsage :: String
helpUsage = usageInfo header theoptions ++ trailer where
    header = "Usage: jhc [OPTION...] Main.hs"
    trailer = "\n" ++ mkoptlist "-d" FD.helpFlags ++ "\n" ++ mkoptlist "-f" FO.helpFlags
    mkoptlist d os = "valid " ++ d ++ " arguments: 'help' for more info\n    " ++ intercalate "\n    " (map (intercalate ", ") $ pfill 80 ((2 +) . length) os) ++ "\n"
    pfill ::
        Int            -- ^ maximum width
        -> (a -> Int)  -- ^ find width of any element
        -> [a]         -- ^ input elements
        -> [[a]]       -- ^ output element
    pfill maxn length xs = f maxn xs [] [] where
        f n (x:xs) ws ls | lx < n = f (n - lx) xs (x:ws) ls where
            lx = length x
        f _ (x:xs) [] ls = f (maxn - length x) xs [x] ls
        f _ (x:xs) ws ls = f (maxn - length x) xs [x] (ws:ls)
        f _ [] [] ls = reverse (map reverse ls)
        f _ [] ws ls = reverse (map reverse (ws:ls))


