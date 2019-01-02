module Language.Grin.Data.Config (
  CfgSwitches(..), GrinConfig(..), HasGrinConfig(..),
  defaultGrinConfig,
  isGrinSwitchEnabled,
  grinVersion
  ) where

import Data.EnumSet.EnumSmallBitSet as EBSet -- containers-missing
import GHC.Exts (IsString(..))


data CfgSwitches
  = CfgLintCheck
  | CfgKeepGoing
  | CfgDebugDupFunctions | CfgDumpDatalog | CfgDumpGrin
  deriving (Show, Eq, Ord, Enum)

data GrinConfig = GrinConfig {
  cfgSwitches   :: EBSet.EnumBitSet64 CfgSwitches
  }


class HasGrinConfig env where grinConfig :: env -> GrinConfig
instance HasGrinConfig GrinConfig where grinConfig = id

defaultGrinConfig :: GrinConfig
defaultGrinConfig = GrinConfig {
  cfgSwitches = EBSet.fromList [CfgLintCheck, CfgDebugDupFunctions]
  }


isGrinSwitchEnabled :: HasGrinConfig env => CfgSwitches -> env -> Bool
isGrinSwitchEnabled sw = EBSet.member sw . cfgSwitches . grinConfig


grinVersion :: IsString str => str
grinVersion = fromString "seagull-kamome/jhc:HEAD - based Jhc:0.8.2"


-- vim: ts=8 sw=2 expandtab :


