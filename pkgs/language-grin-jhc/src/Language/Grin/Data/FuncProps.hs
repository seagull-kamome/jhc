module Language.Grin.Data.FuncProps (
  FuncProps(..)
  ) where

import qualified Data.EnumSet.EnumSet as ESet
import qualified Data.Set as Set

import Language.Grin.AST.Var
import Language.Grin.AST.Tag
import Language.Grin.AST.Type
import Language.Grin.Data.Perhaps

-- ---------------------------------------------------------------------------

data FuncProps sym primtypes = FuncProps {
  --funcInfo    :: !Info.Info,
  funcFreeVars :: !(ESet.EnumSet Var),
  funcTags     :: !(Set.Set (Tag sym)),
  funcType     :: !([Typ primtypes], [Typ primtypes]),
  funcCuts     :: !Perhaps,
  funcAllocs   :: !Perhaps,
  funcCreates  :: !Perhaps,
  funcLoops    :: !Perhaps
  }
  -- deriving (Show, Eq, Ord)


-- ---------------------------------------------------------------------------




-- vim: ts=8 sw=2 expandtab :


