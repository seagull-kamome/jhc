module Language.Grin.Data.TypeEnv (
  TypeEnv(..), findTypeOfType, findArgType, findArg
  ) where

import qualified Data.Map.Strict as Map

import Language.Grin.AST.Type
import Language.Grin.AST.Tag


-- ---------------------------------------------------------------------------


newtype TypeEnv sym primtypes
  = TypeEnv { fromTypeEnv :: Map.Map (Tag sym) (TypeOfType sym primtypes) }
  deriving (Semigroup, Monoid)


findTypeOfType :: (Show (Tag sym), Monad m)
               => TypeEnv sym primtypes
               -> Tag sym
               -> m (TypeOfType sym primtypes)
findTypeOfType (TyEnv x) y =
  case Map.lookup y x of
    Just z -> pure z
    Nothing -> case y of
      TagTypeApp c n -> case lookup x c of
        Just (TypOfType ts tr _ _) ->
          pure $ def { typeSlots = take (length ts - n) ts, typReturn =  tr }
        Nothing -> fail $ "findArgTypes: " <> show y
      TagHole _ -> pure $ def { typReturn = [TypNode] }
      _ -> fail $ "findArgType: " <> show y




findArgType :: Monad m
            => TypeEnv sym primtypes -> sym
            -> m ([Typ primtypes], TypeOfTypes sym primtypes)
findArgType x y = (\TypeType{..} -> (typSlots, typReturn)) <$> findTypeOfType x y




findArg :: Monad m => TypeEnv sym primtypes -> sym -> m [Typ primtypes]
findArgs x y = fst <$> findArgType x y


-- vim: ts=8 sw=2 expandtab :

