module Language.Grin.Data.TypeEnv (
  TypeEnv(..), findTypeOfType --, findArgType, findArgs
  ) where

import qualified Data.Map.Strict as Map

import Language.Grin.AST.Type
import Language.Grin.AST.Tag
import Language.Grin.Data.FuncDef


-- ---------------------------------------------------------------------------


newtype TypeEnv sym primtypes
  = TypeEnv { fromTypeEnv :: Map.Map (Tag sym) (TypeOfType sym primtypes) }
  deriving (Semigroup, Monoid)


findTypeOfType :: (Show (Tag sym), Ord sym, Monad m)
               => Tag sym
               -> TypeEnv sym primtypes
               -> m (TypeOfType sym primtypes)
findTypeOfType y (TypeEnv x) =
  case Map.lookup y x of
    Just z -> pure z
    Nothing -> case y of
      Tag (TagTypePApp c n) -> case Map.lookup (Tag c) x of
        Just (TypeOfType ts tr _ _) ->
          pure $ emptyTypeOfType { typSlots = take (length ts - fromIntegral n) ts, typReturn =  tr }
        Nothing -> fail $ "findArgTypes: " <> show y
      Tag (TagHole _) -> pure $ emptyTypeOfType { typReturn = [TypNode] }
      _ -> fail $ "findArgType: " <> show y


extendTyEnv :: [FuncDef sym primtypes primval expr]
            -> TyEnv sym primtypes
            -> TyEnv sym primtypes
extendTyEnv ds (TyEnv env) = TyEnv (fromList xs `mappend` env)
  where
    xs = [ (funcDefName d,
            emptyTypeOfType { tySlots = ss, tyReturn = r })
          |  d <- ds, let (ss,r) = funcType $ funcDefProps d ]
      ++ [ (tagFlipFunction (funcDefName d), 
           emptyTypeOfType { tySlots = ss, tyReturn = r })
          |  d <- ds, let (ss,r) = funcType $ funcDefProps d, r == [TyNode]]




-- vim: ts=8 sw=2 expandtab :

