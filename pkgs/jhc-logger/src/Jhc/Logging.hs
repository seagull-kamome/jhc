{-
Copyright Hattori, Hiroki (c) 2018

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Hattori, Hiroki nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
module Jhc.Logging (
  LogSource, LogLevel(..), LogFunc (..),
  HasLogFunc(..),
  --
  defaultColor, defaultLogSend, defaultLogFunc, defaultTimestampFormatter,
  setLogUseTimestamp, setLogMinLevel, setLogUseColor, setLogTerminal,
  setLogSender, setLogFatalHook,
  setLogExitOnFatal,setLogDiesOnFatal, setLogErrorOnFatal, setLogColumn,
  --
  sendLog', sendLog,
  --
  logDebug, logInfo, logNotice, logWarn, logError, logFatal,
  logErrorFail, logFatalFail,
  --
  MonadLogging(..),
  logDebugM, logInfoM, logNoticeM, logWarnM, logErrorM, logFatalM,
  logErrorFailM, logFatalFailM
  ) where

import qualified Data.Text as T
import Control.Monad (when)
import Control.Monad.Fail (MonadFail(fail))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader.Class (MonadReader(..))
import System.IO (Handle, stderr)
import System.Exit (exitWith, ExitCode, die)

import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Prelude hiding(fail)

-- ---------------------------------------------------------------------------

type LogSource = T.Text
data LogLevel = LevelDebug | LevelInfo | LevelNotice
              | LevelWarn | LevelError | LevelFatal
  deriving (Eq, Ord, Show, Read)
instance Pretty LogLevel where
  pretty = \case
    LevelDebug -> text "debug"
    LevelInfo -> text "info"
    LevelNotice -> text "notice"
    LevelWarn -> text "warning"
    LevelError -> text "error"
    LevelFatal -> text "FATAL"

data LogFunc = LogFunc {
  logMinLevel :: !LogLevel,
  logUseColor :: !Bool,
  logTerminal :: !Bool,
  logColumn :: !(Maybe (Float, Int)),
  logUseTimestamp :: !Bool,
  logSend :: !(Maybe UTCTime -> LogSource -> LogLevel -> Doc -> LogFunc -> IO ()),
  logFatalHook :: !(Doc -> IO ())
  }

instance Semigroup LogFunc where
  x <> y = LogFunc {
    logMinLevel = max (logMinLevel x) (logMinLevel y),
    logUseColor = logUseColor x || logUseColor y,
    logTerminal = logTerminal x || logTerminal y,
    logColumn = Nothing,
    logUseTimestamp = logUseTimestamp x || logUseTimestamp y,
    logSend = \x1 x2 x3 x4 x5 -> logSend x x1 x2 x3 x4 x5 >> logSend y x1 x2 x3 x4 x5,
    logFatalHook = \x1-> logFatalHook x x1 >> logFatalHook y x1
  }
instance Monoid LogFunc where
  mempty = LogFunc {
    logMinLevel = LevelFatal,
    logUseColor = False,
    logTerminal = False,
    logColumn = Nothing,
    logUseTimestamp = False,
    logSend = \_ _ _ _ _ -> return (),
    logFatalHook = \_ -> return ()
  }

-- ---------------------------------------------------------------------------


class HasLogFunc a where getLogFunc :: a -> LogFunc
instance HasLogFunc LogFunc where
  getLogFunc = id
  {-# INLINE getLogFunc #-}

class MonadLogging m where getLogFuncM :: m LogFunc

--instance (Monad m, MonadReader env m, HasLogFunc env) => MonadLogging m where
--  getLogFuncM = getLogFunc <$> ask
--  {-# INLINE getLogFuncM #-}
--instance (Monad m, MonadState env m, HasLogFunc env) => MonadLogging m where
--  getLogFuncM = getLogFUnc <$> get
--  {-# INLINE getLogFuncM #-}

-- ---------------------------------------------------------------------------

defaultColor :: LogLevel -> Doc -> Doc
defaultColor LevelDebug = dullwhite
defaultColor LevelInfo = dullwhite
defaultColor LevelNotice = white
defaultColor LevelWarn = dullred
defaultColor LevelError = red
defaultColor LevelFatal = red
{-# INCLUDE defaultColor #-}

defaultTimestampFormatter :: Maybe UTCTime -> Doc
defaultTimestampFormatter Nothing = empty
defaultTimestampFormatter (Just x) = text $ formatTime defaultTimeLocale "%x %X" x

defaultLogSend :: Handle -> Maybe UTCTime -> LogSource -> LogLevel -> Doc -> LogFunc -> IO ()
defaultLogSend h ts src lvl msg LogFunc{..} = displayIO h sdoc
  where
    doc = defaultTimestampFormatter ts <> colon
          <+> text (T.unpack src) <> colon
          <+> defaultColor lvl (pretty lvl) <> colon
          <+> msg
    sdoc = maybe renderCompact (uncurry renderPretty) logColumn
             $ if logUseColor && logTerminal then doc else plain doc

defaultLogFunc :: LogFunc
defaultLogFunc = LogFunc {
    logMinLevel = LevelDebug,
    logUseColor = True,
    logTerminal = True,
    logColumn = Nothing,
    logUseTimestamp = True,
    logSend = defaultLogSend stderr,
    logFatalHook = const (pure ())
  }


setLogMinLevel :: LogLevel -> LogFunc -> LogFunc
setLogMinLevel x y = y { logMinLevel = x }
{-# INLINE setLogMinLevel #-}

setLogUseColor :: Bool -> LogFunc -> LogFunc
setLogUseColor x y = y { logUseColor = x }
{-# INLINE setLogUseColor #-}

setLogColumn :: Maybe (Float, Int) -> LogFunc -> LogFunc
setLogColumn x y = y { logColumn = x }
{-# INLINE setLogColumn #-}

setLogTerminal :: Bool -> LogFunc -> LogFunc
setLogTerminal x y = y { logTerminal = x }
{-# INLINE setLogTerminal #-}

setLogUseTimestamp :: Bool -> LogFunc -> LogFunc
setLogUseTimestamp x y = y { logUseTimestamp = x }
{-# INLINE setLogUseTimestamp #-}

setLogSender :: (Maybe UTCTime -> LogSource -> LogLevel -> Doc -> LogFunc -> IO ()) -> LogFunc -> LogFunc
setLogSender x y = y { logSend = x }
{-# INLINE setLogSender #-}

setLogFatalHook :: (Doc -> IO ()) -> LogFunc -> LogFunc
setLogFatalHook x y = y { logFatalHook = x }
{-# INLINE setLogFatalHook #-}

setLogExitOnFatal :: ExitCode -> LogFunc -> LogFunc
setLogExitOnFatal ec y = y { logFatalHook = \x -> hPutDoc stderr x >> exitWith ec }

setLogDiesOnFatal :: LogFunc -> LogFunc
setLogDiesOnFatal y = y { logFatalHook = \m -> die $ displayS (renderCompact $ plain m) "" }

setLogErrorOnFatal :: LogFunc -> LogFunc
setLogErrorOnFatal y = y { logFatalHook = \m -> error $ displayS (renderCompact $ plain m) "" }


-- ---------------------------------------------------------------------------

sendLog' :: (MonadIO m, HasLogFunc env)
         => LogSource -> LogLevel -> Doc -> env-> m ()
sendLog' src lvl msg e = do
  let fnc@LogFunc {..} = getLogFunc e
  liftIO $ when (logMinLevel <= lvl) $ do
    ts <- if logUseTimestamp then Just <$> getCurrentTime else return Nothing
    logSend ts src lvl msg fnc
    when (lvl == LevelFatal) $ logFatalHook msg


sendLog :: (MonadIO m, MonadReader env m, HasLogFunc env)
         => LogSource -> LogLevel -> Doc -> m ()
sendLog src lvl msg = ask >>= sendLog' src lvl msg . getLogFunc

-- ---------------------------------------------------------------------------

logDebug, logInfo, logNotice, logWarn, logError, logFatal
  :: (MonadIO m, MonadReader env m, HasLogFunc env) => LogSource -> Doc -> m ()
logDebug s = sendLog s LevelDebug
{-# INLINE logDebug #-}
logInfo s = sendLog s LevelInfo
{-# INLINE logInfo #-}
logNotice s = sendLog s LevelNotice
{-# INLINE logNotice #-}
logWarn s = sendLog s LevelWarn
{-# INLINE logWarn #-}
logError s = sendLog s LevelError
{-# INLINE logError #-}
logFatal s = sendLog s LevelFatal
{-# INLINE logFatal #-}



logErrorFail, logFatalFail
  :: (MonadIO m, MonadFail m, MonadReader env m, HasLogFunc env) => LogSource -> Doc -> m a
logErrorFail x y = logError x y >> fail (T.unpack x)
{-# INLINE logErrorFail #-}
logFatalFail x y = logFatal x y >> fail (T.unpack x)
{-# INLINE logFatalFail #-}



-- ---------------------------------------------------------------------------

logDebugM, logInfoM, logNoticeM, logWarnM, logErrorM, logFatalM
  :: (MonadIO m, MonadLogging m) => LogSource -> Doc -> m ()
logDebugM s m = getLogFuncM >>= liftIO . sendLog' s LevelDebug m
{-# INLINE logDebugM #-}
logInfoM s m = getLogFuncM >>= liftIO . sendLog' s LevelInfo m
{-# INLINE logInfoM #-}
logNoticeM s m = getLogFuncM >>= liftIO . sendLog' s LevelNotice m
{-# INLINE logNoticeM #-}
logWarnM s m = getLogFuncM >>= liftIO . sendLog' s LevelWarn m
{-# INLINE logWarnM #-}
logErrorM s m = getLogFuncM >>= liftIO . sendLog' s LevelError m
{-# INLINE logErrorM #-}
logFatalM s m = getLogFuncM >>= liftIO . sendLog' s LevelFatal m
{-# INLINE logFatalM #-}


logErrorFailM, logFatalFailM
  :: (MonadIO m, MonadFail m, MonadLogging m) => LogSource -> Doc -> m a
logErrorFailM s m = getLogFuncM >>= liftIO . sendLog' s LevelError m
                    >> fail (T.unpack s)
{-# INLINE logErrorFailM #-}
logFatalFailM s m = getLogFuncM >>= liftIO . sendLog' s LevelFatal m
                    >> fail (T.unpack s)
{-# INLINE logFatalFailM #-}



-- vim: ts=8 sw=2 expandtab :

