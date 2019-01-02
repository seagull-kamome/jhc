module Language.Grin.Data.TypeEnv (
  TypeEnv(..), findTypeOfType, -- findArgType, findArgs
  extendTyEnv
  ) where

import qualified Data.Map.Strict as Map

import Language.Grin.AST.Type
import Language.Grin.AST.Tag
import Language.Grin.Data.FuncDef
import Language.Grin.Data.FuncProps


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

