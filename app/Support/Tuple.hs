module Support.Tuple {-# DEPRECATED "DON'T USE THESE" #-} where

import Data.List (intercalate)

class Tuple a where
    tupleNil :: a
    tupleOne :: a -> a
    tupleMany :: [a] -> a

    tupleNil = tupleMany []
    tupleOne x = x

class FromTuple a where
    fromTuple :: a -> [a]

tuple :: Tuple a => [a] -> a
tuple [] = tupleNil
tuple [x] = tupleOne x
tuple xs = tupleMany xs

instance Tuple String where
    tupleMany xs = "(" ++ intercalate "," xs ++ ")"

