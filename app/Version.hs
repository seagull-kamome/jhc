{-# LANGUAGE OverloadedStrings #-}
module Version (
  versionMajor, versionMinor, versionPatch, revision,
  packageName,
  libDir, dataDir, confDir, libraryInstall,
  host, hoVersion,
  shortVersion, version, 
  versionSimple, versionText
  ) where

import Data.List (intercalate)
import Data.Version (showVersion)
import qualified Data.Text as T
import qualified System.Info as SysInfo

-- ---------------------------------------------------------------------------
-- Version configuration

versionMajor, versionMinor, versionPatch :: Int
versionMajor = 0
versionMinor = 8
versionPatch = 2

packageName :: T.Text
packageName = "jhc"

libDir, dataDir, confDir, libraryInstall :: T.Text
libDir = "/usr/local/lib"            -- TODO:
dataDir = "/usr/local/share"         -- TODO:
confDir = "/usr/local/etc/jhc-0.8"   -- TODO:
libraryInstall = "/usr/local/share/jhc-0.8" -- TODO:

host :: T.Text
host = "x86_64-unknown-linux-gnu"    -- TODO:

hoVersion :: Int
hoVersion = 14



-- ---------------------------------------------------------------------------
-- Utilities

revision :: Int
revision = versionMajor * 10000 + versionMinor * 100 + versionPatch

shortVersion, version:: T.Text
shortVersion = T.pack $ intercalate "." $ map show [ versionMajor, versionMinor ]
version = T.pack $ intercalate "." $ map show [ versionMajor, versionMinor, versionPatch ]

versionSimple, versionText :: T.Text
versionSimple = packageName <> " " <> version
versionText = versionSimple <> "=ncompiled by "
   <> T.pack (SysInfo.compilerName
       ++ "-" ++ showVersion SysInfo.compilerVersion
       ++ " on a " ++ SysInfo.arch
       ++ " running " ++ SysInfo.os)


