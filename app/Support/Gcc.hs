{-# LANGUAGE ViewPatterns,RecordWildCards #-}
-- Collect information about target by interogating a C compiler
module Support.Gcc(
    Compiler(),
    findCompiler,
    targetBoolValue,
    targetIntValue,
    targetIndirectBoolValue,
    compileTest
    ) where

import Control.Monad
import Data.Binary
import Data.IORef
import Data.Maybe
import Data.Monoid(Monoid(..))
import System.Directory
import System.Exit
import System.FilePath as FP
import System.IO
import System.Process
import Text.Printf
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Map as Map

import Support.MD5
import Support.TempDir

salt :: LBS.ByteString
salt = LBS8.pack "v1"

-- A compiler is uniquely determined by its path, a hash of its version output
-- and any flags that affect its compilation mode.
data Compiler = Compiler {
    compilerPath :: FilePath,
    compilerArgs :: [String],
    compilerHash :: Hash,
    compilerData :: {-# UNPACK #-} !(IORef CompilerData)
    }

data CompilerData = CD {
    cdCompileTest  :: Map.Map ([BS.ByteString], BS.ByteString) Bool,
    cdCompileInt   :: Map.Map ([BS.ByteString], BS.ByteString) Integer
    }
instance Data.Binary.Binary CompilerData where
    put (CD aa ab) = Data.Binary.put aa >> Data.Binary.put ab
    get = pure CD <*> get <*> get
instance Semigroup CompilerData where
    (CD aa ab) <> (CD aa' ab') = CD (aa <> aa') (ab <> ab')
instance Monoid CompilerData where
    mempty = CD mempty mempty

emptyCompilerData = mempty



findCompiler
    :: [FilePath]     -- ^ compilers to search for, will use first one found.
    -> [String]       -- ^ arguments to pass to compiler.
    -> IO Compiler
findCompiler cs as = do
    let check (c:cs) = do
            mfp <- findExecutable c
            maybe (check cs) return mfp
        check [] = fail $ "Could not find C compiler among " ++ show cs
    comp <- check cs
    (Nothing,Just out,Nothing,ph) <-
        createProcess $ (proc comp ("-dM":"-E":"-xc":"/dev/null":as))
            { std_out = CreatePipe }
    versOut <- LBS.hGetContents out
    hash <- md5lazyIO $ LBS.concat [salt, versOut, LBS8.pack $ show as]
    code <- waitForProcess ph
    when (code /= ExitSuccess) $
        fail $ printf "Compiler '%s' exited with error code %s" comp (show code)
    printf "found complier %s with hash %s\n" comp (show hash)
    ref <- newIORef emptyCompilerData
    return Compiler {
            compilerPath = comp,
            compilerArgs = as,
            compilerHash = hash,
            compilerData = ref
        }

targetIndirectBoolValue :: Compiler -> String -> IO Bool
targetIndirectBoolValue Compiler { .. } s = do
    (fp,h) <- createTempFile "test.c"
    hPutStr h (indirectTemplate s)
    hClose h
    devNull <- openDevNull
    (Nothing,Nothing,Nothing,ph) <- createProcess $ (proc compilerPath $ "-c":fp:compilerArgs)
            { std_out = UseHandle devNull , std_err = UseHandle devNull }
    ec <- waitForProcess ph
    return (ec == ExitSuccess)
  where
    indirectTemplate c =
      "#include <stdio.h>\n" ++
      "#include <stdint.h>\n" ++
      "\n" ++
      "int test[(!!(" ++ c ++ ")) - 1];\n"




targetBoolValue :: Compiler -> String -> IO Bool
targetBoolValue c s = do
    i <- targetIntValue c ("!!(" ++ s ++ ")")
    return $ i == 1

targetIntValue :: Compiler -> String -> IO Integer
targetIntValue Compiler { .. } s = do
    (fp,h) <- createTempFile "test.c"
--    printf "creating temporary file %s to find value of %s" fp s
    hPutStr h (directTemplate s)
    hClose h
    td <- getTempDir
    let bname = td </> takeBaseName fp
    let args = "-o":bname:fp:compilerArgs
    devNull <- openDevNull
    ec <- rawSystem compilerPath args
    when (ec /= ExitSuccess) $
        fail $ printf "error running %s %s to determine value of %s"
            compilerPath (unwords args) s
    (Nothing,Just out,Nothing,ph) <- createProcess $ (proc bname [])
            { std_out = CreatePipe, std_err = UseHandle devNull }
    bs <- BS.hGetContents out
    ec <- waitForProcess ph
    hClose devNull
    when (ec /= ExitSuccess) $ fail $ printf
        "error running %s to determine value of %s, failed with %s"
        bname s (show ec)
    return (read (BS8.unpack bs))
  where
    directTemplate c =
      "#include <stdio.h>\n" ++
      "#include <stdint.h>\n" ++
      "\n" ++
      "int main(int argc, char* argv[]) {\n" ++
      "printf(\"%lu\", (long)(" ++ c ++ "));\n" ++
      "\n" ++
      "return 0;\n" ++
      "}\n"




compileTest :: Compiler -> [String] -> String -> IO Bool
compileTest c@Compiler { .. } ss s = do
    let key = (map BSU.fromString ss, BSU.fromString s)
    ct <- cdCompileTest `liftM` readIORef compilerData
    case Map.lookup key ct of
        Just r -> return r
        Nothing -> do
            res <- compileTest' c key
            modifyIORef compilerData (\ cd@CD { .. } -> cd { cdCompileTest =  Map.insert key res cdCompileTest })
            return res

compileTest' :: Compiler -> ([BS.ByteString],BS.ByteString) -> IO Bool
compileTest' Compiler { .. } (ss,s) = do
    (fp,h) <- createTempFile "comptest.c"
    hPutStr h $ BSU.toString s
    hClose h
    td <- getTempDir
    let args = map BSU.toString ss ++ "-c":fp:"-o":(td </> "dummy.o"):compilerArgs
    devNull <- Just `liftM` openDevNull
    ph <- runProcess compilerPath args Nothing Nothing devNull devNull devNull
    ec <- waitForProcess ph
    hClose (fromJust devNull)
    return (ec == ExitSuccess)

openDevNull = openFile "/dev/null" ReadWriteMode



