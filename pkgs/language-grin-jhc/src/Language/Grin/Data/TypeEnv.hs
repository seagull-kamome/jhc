module Language.Grin.Data.TypeEnv (
  TypeEnv(..), HasTypeEnv(..),
  findTypeOfType, -- findArgType, findArgs
  extendTyEnv
  ) where

import Prelude hiding(fail)

import qualified Data.Map.Strict as Map
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.Reader.Class

import Language.Grin.AST.Type
import Language.Grin.AST.Tag
import Language.Grin.Data.FuncDef
import Language.Grin.Data.FuncProps


-- ---------------------------------------------------------------------------


newtype TypeEnv sym primtypes
  = TypeEnv { fromTypeEnv :: Map.Map (Tag sym) (TypeOfType sym primtypes) }
  deriving (Semigroup, Monoid)

class HasTypeEnv env sym primtypes | env -> sym primtypes where
  getTypeEnv :: env -> TypeEnv sym primtypes


-- ---------------------------------------------------------------------------


findTypeOfType :: (Ord sym,
                   Monad m, MonadFail m,
                   MonadReader env m, HasTypeEnv env sym primtypes)
               => Tag sym
               -> m (TypeOfType sym primtypes)
findTypeOfType y = do
  TypeEnv x <- asks getTypeEnv
  case Map.lookup y x of
    Just z -> pure z
    Nothing ->  case y of
        Tag (TagTypePApp c n) -> case Map.lookup (Tag c) x of
          Just (TypeOfType ts tr _ _) ->
            pure $ emptyTypeOfType { typSlots = take (length ts - fromIntegral n) ts, typReturn =  tr }
          Nothing -> fail "findArgTypes: "
        Tag (TagHole _) -> pure $ emptyTypeOfType { typReturn = [TypNode] }
        _ -> fail "findArgType: "



-- findArgTypes :: Tag sym -> TypeEnv sym primtypes -> Typ primtypes
-- findArgsType m a = liftM (\x -> (typSlots x,typReturn x)) (findTypeOfType m a)
-- findArgs m a = case findArgsType m a of
--    Nothing -> fail $ "findArgs: " ++ show a
--    Just (as,_) -> return as




extendTyEnv :: (Ord sym, Eq primtypes)
            => [FuncDef sym primtypes primval expr]
            -> TypeEnv sym primtypes
            -> Either [Tag sym] (TypeEnv sym primtypes)  -- error symbols or new env.
extendTyEnv ds (TypeEnv env) = f [] [] ds
  where
    xs = [ (funcDefName d,
            emptyTypeOfType { typSlots = ss, typReturn = r })
          |  d <- ds, let (ss,r) = funcType $ funcDefProps d ]
    f rs []  [] = Right $ TypeEnv (Map.fromList (xs ++ rs) <> env)
    f _  err [] = Left err
    f rs err (y:ys) =
      let (!ss, !r) = funcType $ funcDefProps y
          e = emptyTypeOfType { typSlots = ss, typReturn = r}
          !name = funcDefName y
          !name' = tagFlipFunction name
       in if r /= [TypNode] 
             then f rs err ys
             else maybe (f rs (err ++ [name]) ys)
                        (\n -> f (rs ++ [(n, e)]) err ys) name'


-- ---------------------------------------------------------------------------

-- vim: ts=8 sw=2 expandtab :

