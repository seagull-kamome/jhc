module Language.Grin.AST.Program (
  OptimizingPhase, Program(..), emptyProgram
  ) where


import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Language.Grin.AST.Var
import Language.Grin.AST.Val
import Language.Grin.AST.Expression
import Language.Grin.Data.TypeEnv
import Language.Grin.Data.FFIExport



-- ---------------------------------------------------------------------------

data OptimizingPhase
  = OPhaseInit | OPhasePostInlineEval | OPhaseAeOptimize | OPhasePostDevolve
  deriving (Show, Eq, Ord, Enum)


data Program sym primtypes primopr primval
  = Program {
    progEntryPoints :: !(Map.Map sym (FFIExport sym)),
    progOptimizingPhase :: !OptimizingPhase,
    progTypeEnv :: !(TypeEnv sym primtypes),
    progFunctions :: ![FuncDef sym primtypes primval (Expression sym primtypes primopr primval)],
    progSuspFunctions :: !(Set.Set sym),
    progPartFUnctions :: !(Set.Set sym),
    progCafs :: ![(Var, Val sym primtypes primval)]
    }



-- ---------------------------------------------------------------------------
-- Construction

emptyProgram :: Ord sym => Program sym primtypes primopr primval
emptyProgram = Program mempty OPhaseInit mempty mempty mempty mempty mempty



  {-
setGrinFunctions xs _grin | flint && hasRepeatUnder fst xs
  = error $ "setGrinFunctions: grin has redundent definitions" ++ show (fsts xs)
setGrinFunctions xs grin
  = grin { grinFunctions = map (uncurry (newFuncDef False)) xs }
-}



-- vim: ts=8 sw=2 expandtab :

