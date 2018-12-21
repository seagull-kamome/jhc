module Language.Grim.AST.Program (
  TypeEnv(..), findTypeOfType,
  OptimizingPhase, Program(..), emptyProgram
  ) where


import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set

import Language.Grin.AST.Val
import Language.Grin.AST.Type
import Language.Grin.AST.Expression


-- ---------------------------------------------------------------------------


newtype TypeEnv sym primtypes
  = TypeEnv ( fromTypeEnv :: Map.Map sym (TypeOfType sym primtypes) }
  deriving (Semigroup, Monoid)


// TODO:
findTypeOfType :: TypeEnv sym primtypes -> sym -> Either T.Text (TypeOfType sym primtypes)
findArgType :: TypeEnv sym primtypes -> sym
            -> Either T.Text ([Typ primtypes], (TypeOfTypes sym primtypes))
findArg :: TypeEnv sym primtypes -> sym -> Either T.Text ([Typ primtypes])



-- ---------------------------------------------------------------------------

data OpitimizingPhase
  = OPhaseInit | OPhasePostInlineEval | OPhaseAeOptimize | OPhasePostDevolve
  deriving (Show, Eq, Ord, Enum)



data Program sym primtypes primopr littyp primval
  = Program {
    progEntryPoints :: Map.Map sym FFIExport,
    progOptimizingPhase :: OptimizingPhase,
    progTypeEnv :: TypeEnv sym primtypes,
    progFunctions :: [FuncDef sym primtypes littyp primval (Expression sym primtypes primopr littyps primval)],
    progSuspFunctions :: Set.Set sym,
    progPartFUnctions :: Set.Set sym,
    progCafs :: [(Var, Val sym primtypes littyp primval)]
    }



-- ---------------------------------------------------------------------------
-- Construction

emptyProgram :: Program sym primtypes primopr littyp primval
emptyProgram = Program {
  mempty, OPhaseInit, mempty, mempty, mempty, mempty, mempty }






