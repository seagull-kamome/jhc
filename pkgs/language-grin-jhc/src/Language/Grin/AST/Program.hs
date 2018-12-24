module Language.Grin.AST.Program (
  OptimizingPhase, Program(..), emptyProgram
  ) where


import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set

import Language.Grin.AST.Val
import Language.Grin.AST.Type
import Language.Grin.AST.Expression



-- ---------------------------------------------------------------------------

data OpitimizingPhase
  = OPhaseInit | OPhasePostInlineEval | OPhaseAeOptimize | OPhasePostDevolve
  deriving (Show, Eq, Ord, Enum)



data Program sym primtypes primopr primval
  = Program {
    progEntryPoints :: Map.Map sym FFIExport,
    progOptimizingPhase :: OptimizingPhase,
    progTypeEnv :: TypeEnv sym primtypes,
    progFunctions :: [FuncDef sym primtypes primval (Expression sym primtypes primopr primval)],
    progSuspFunctions :: Set.Set sym,
    progPartFUnctions :: Set.Set sym,
    progCafs :: [(Var, Val sym primtypes primval)]
    }



-- ---------------------------------------------------------------------------
-- Construction

emptyProgram :: Program sym primtypes primopr primval
emptyProgram = Program mempty OPhaseInit mempty mempty mempty mempty mempty




-- vim: ts=8 sw=2 expandtab :

