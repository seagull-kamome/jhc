module Options.Parser.CmdArgs (
  parser, showUsage
  ) where

import qualified Data.Text as T
import System.Console.GetOpt

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Options.Types

-- ---------------------------------------------------------------------------

{-# NOINLINE theoptions #-}
parser :: [OptDescr (Opt -> Either T.Text Opt)]
parser =
  [Option ['V'] ["version"]         (NoArg  (\x -> Right $ x { optMode = Version })_          "print version info and exit"
  ,Option ['h'] ["help"]            (NoArg  (\x -> Right $ x { optMode = ShowHelp }))         "print help information and exit"
  ,Option []    ["info"]            (NoArg  (\x -> Right $ x { optMode = ShowConfig }))       "show compiler configuration information and exit"
  ,Option []    ["purge-cache"]     (NoArg  (\x -> Right $ x { optMode = PurgeCache }))       "clean out jhc compilation cache"
  ,Option ['v'] ["verbose"]         (NoArg  (\x -> Right $ x { optVerbose = optVerbose x + 1}))          "chatty output on stderr"
  ,Option ['z'] []                  (NoArg  (\x -> Right $ x { optStatLevel = optStatLevel x + 1 }))        "Increase verbosity of statistics"
  ,Option ['d'] []                  (ReqArg (\y x -> Right $ x { optDump = y:optDump x }) "[no-]flag") "dump specified data during compilation"
  ,Option ['f'] []                  (ReqArg (\y x -> Right $ x { optFOpts = y:optFOpts x }) "[no-]flag") "set or clear compilation options"
  ,Option ['X'] []                  (ReqArg (\y x -> Right $ x { optExtensions = y:optExtensions x })  "Extension") "enable the given language extension by the cabal name. see -fhelp for jhc native names."
  ,Option ['o'] ["output"]          (ReqArg (\y x -> Right $ x { optOutName = Just y })  "FILE") "output to FILE"
  ,Option ['i'] ["include"]         (ReqArg (\y x -> Right $ x { optIncdirs = idu y $ optIncdirs x }) "DIR")   "path to look for haskell source files. use -i- to clear the search path."
  ,Option ['I'] []                  (ReqArg (\y x -> Right $ x { optIncs = idu y $ optIncs x }) "DIR")       "add to preprocessor include path"
  ,Option []    ["with"]            (ReqArg (\y x -> Right $ x { optWith = idu y $ optWith x }) "foo.yaml") "include values from yaml file in configuration"
  ,Option ['D'] []                  (ReqArg (\y x -> Right $ x { optDefs = y:optDefs x }) "NAME=VALUE") "add new definitions to set in preprocessor"
  ,Option []    ["optc"]            (ReqArg (\y x -> Right $ x { optCCargs = idu y $ optCCargs x }) "option") "extra options to pass to c compiler"
  ,Option ['c'] []                  (NoArg  (\x -> Right $ x { optStop = CompileHo }))        "just compile the modules, caching the results."
  ,Option ['C'] []                  (NoArg  (\x -> Right $ x { optStop = StopC }))            "compile to C code"
  ,Option ['E'] []                  (NoArg  (\x -> Right $ x { optMode = Preprocess }))       "preprocess the input and print result to stdout"
  ,Option ['k'] ["keepgoing"]       (NoArg  (\x -> Right $ x { optKeepGoing = True }))        "keep going on errors"
  ,Option []    ["cross"]           (NoArg  (\x -> Right $ x { optCross =  True }))            "enable cross-compilation, choose target with the -m flag"
  ,Option []    ["stop"]            (ReqArg (\y x -> Right $ x { optStop = stop y }) "parse/typecheck/c") "stop after the given pass, parse/typecheck/c"
  ,Option []    ["width"]           (ReqArg (\y x -> Right $ x { optColumns = read y})  "COLUMNS") "width of screen for debugging output"
  ,Option []    ["main"]            (ReqArg (\y x -> Right $ x { optMainFunc = Just (False, y) }) "Main.main")  "main entry point"
  ,Option ['m'] ["arch"]            (ReqArg (\y x -> Right $ x { optArch = idu y $ optArch x }) "arch")      "target architecture options"
  ,Option []    ["entry"]           (ReqArg (\y x -> Right $ x { optMainFunc = Just (True, y) })  "<expr>")  "main entry point, showable expression"
  ,Option ['e'] []                  (ReqArg (\d -> optStmts_u ( d:)) "<statement>")  "run given statement as if on jhci prompt"
  ,Option []    ["show-ho"]         (ReqArg (\y x -> Right $ x { optMode = ShowHo y }) "file.ho") "Show contents of ho or hl file. Use -dflag to determine what information to show."
  ,Option []    ["noauto"]          (NoArg  (\x ->Right $  x { optNoAuto =  True }))           "Do not automatically depend on primitive libraries. Used to build jhc-prim. Use -p- to clear the visible package list."
  ,Option ['p'] ["package"]         (ReqArg (\y x -> Right $ x { optHls = y:optHls x }) "package")   "Load given haskell library package. Use -p- to remove all current entries from the list."
  ,Option ['L'] []                  (ReqArg (\y x -> Right $ x { optHlPath = idu y $ optHlPath x }) "path")   "Look for haskell libraries in the given directory. Use -L- to clear the search path."
  ,Option []    ["build-hl"]        (ReqArg (\y x -> Right $ x { optMode = BuildHl y }) "desc.yaml") "Build hakell library from given library description file"
  ,Option []    ["annotate-source"] (ReqArg (\y x -> Right $ x { optAnnotate = Just y }) "<dir>") "Write preprocessed and annotated source code to the directory specified"
  ,Option []    ["deps"]            (ReqArg (\y x -> Right $ x { optDeps = Just y }) "<file.yaml>") "Write dependency information to file specified"
  ,Option []    ["interactive"]     (NoArg  (\x -> Right $ x { optMode = Interactive }))      "run interactivly ( for debugging only)"
  ,Option []    ["ignore-cache"]    (NoArg  (\x -> Right $ x { optIgnoreHo = True }))         "Ignore existing compilation cache entries."
  ,Option []    ["readonly-cache"]  (NoArg  (\x -> Right $ x { optNoWriteHo = True }))        "Do not write new information to the compilation cache."
  ,Option []    ["no-cache"]        (NoArg  (\x -> Right $ x { optNoWriteHo = True, optIgnoreHo = True })) "Do not use or update the cache."
  ,Option []    ["cache-dir"]       (ReqArg (\y x -> Right $ x { optHoCache = Just y }) "JHC_CACHE")  "Use a global cache located in the directory passed as an argument."
  ,Option []    ["stale"]           (ReqArg (\y x  -> Right $ x { optStale = idu y $ optStale x }) "Module")  "Treat these modules as stale, even if they exist in the cache."
  ,Option []    ["list-libraries"]  (NoArg  (\x -> Right $ x { optMode = ListLibraries }))    "List of installed libraries."
  ,Option []    ["tdir"]            (ReqArg (\y x -> Right $ x { optWorkDir = Just y }) "dir/") "specify the directory where all intermediate files/dumps will be placed."
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




showUsage :: Handle -> Opts -> IO ()
showUsage h env = hPutDoc h $
  text (usageInfo "Usage: jhc [OPTION...] Main.hs" parser)
    <$$> text (mkoptlist "-d" FD.helpFlags)
    <$$> text (mkoptlist "-f" FO.helpFlags)
  where
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



