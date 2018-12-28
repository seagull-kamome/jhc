module Language.Grin.AST.Tag (
  TagType(..), Tag''(..), Tag(..),
  isSuspFunction, isFunction, isPartialAp, isTag, isWHNF,
  toPartial, toFunction, tagUnfunction, flipFunction
  ) where

import Text.PrettyPrint.ANSI.Leijen hiding((<$>))


-- ---------------------------------------------------------------------------
-- Basic Tag


data TagType = Tag'Hole | Tag'f | Tag'C | Tag'P | Tag'F | Tag'b | Tag'B | Tag'T | Tag'Y

data Tag'' (tagtyp :: TagType) sym where
  TagHole :: !Word -> Tag'' 'Tag'Hole sym
  TagDataCons {- C -} :: !sym -> Tag'' 'Tag'C sym
  TagFunc {- f -} :: !sym -> Tag'' 'Tag'f sym
  TagPApp {- P -} :: !(Tag'' 'Tag'f sym) -> !Word {- /=0 -} -> Tag'' 'Tag'P sym
  TagSusp {- F -} :: !(Tag'' 'Tag'f sym) -> !Bool -> Tag'' 'Tag'F sym
  TagFunc' {- b -} :: !sym -> Tag'' 'Tag'b sym
  TagSusp' {- B -} :: !(Tag'' 'Tag'b sym) -> !Bool -> Tag'' 'Tag'B sym
  TagTypeCons {- T -} :: !sym -> Tag'' 'Tag'T sym
  TagTypePApp {- Y -} :: !(Tag'' 'Tag'T sym) -> !Word {- /= 0 -} -> Tag'' 'Tag'Y sym

deriving instance Show sym => Show (Tag'' tagtyp sym)
deriving instance Eq sym => Eq (Tag'' tagtyp sym)
instance Ord sym => Ord (Tag'' tagtyp sym) where
  compare = compareTag

instance Pretty sym => Pretty (Tag'' tagtyp sym) where
  pretty (TagHole n)     = "@hole_" <> text (show n)
  pretty (TagDataCons x) = "C" <> pretty x
  pretty (TagFunc x)     = "f" <> pretty x
  pretty (TagPApp (TagFunc x) n)   = "P" <> text (show n) <> "_" <> pretty x
  pretty (TagSusp (TagFunc x) _)   = "F" <> pretty x
  pretty (TagFunc' x)    = "b" <> pretty x
  pretty (TagSusp' (TagFunc' x) _)  = "B" <> pretty x
  pretty (TagTypeCons x) = "T" <> pretty x
  pretty (TagTypePApp (TagTypeCons x) _) = "V" <> pretty x




{- PRIVATE -}
compareTag :: Ord sym => Tag'' tt1 sym -> Tag'' tt2 sym -> Ordering
compareTag lhs rhs = case compare (conid lhs) (conid rhs) of
  EQ -> case (lhs, rhs) of
    (TagHole n, TagHole m) -> compare n m
    (TagDataCons s1, TagDataCons s2) -> compare s1 s2
    (TagFunc s1, TagFunc s2) -> compare s1 s2
    (TagPApp t1 n, TagPApp t2 m) -> case compareTag t1 t2 of
      EQ -> compare n m
      x -> x
    (TagSusp t1 b1, TagSusp t2 b2) -> case compareTag t1 t2 of
      EQ -> compare b1 b2
      x -> x
    (TagFunc' s1, TagFunc' s2) -> compare s1 s2
    (TagSusp' t1 b1, TagSusp' t2 b2) -> case compareTag t1 t2 of
      EQ -> compare b1 b2
      x -> x
    (TagTypeCons s1, TagTypeCons s2) -> compare s1 s2
    (TagTypePApp t1 n, TagTypePApp t2 m) -> case compareTag t1 t2 of
      EQ -> compare n m
      x -> x
    _ -> undefined -- Believe me!!
  x -> x
  where
    conid :: Tag'' (tagtyp :: TagType) sym -> Int
    conid (TagHole _) = 0
    conid (TagDataCons _) = 1
    conid (TagFunc _) = 2
    conid (TagPApp _ _) = 3
    conid (TagSusp _ _) = 4
    conid (TagFunc' _) = 5
    conid (TagSusp' _ _) = 6
    conid (TagTypeCons _) = 7
    conid (TagTypePApp _ _) = 8



-- ---------------------------------------------------------------------------
-- Quantified Tag

data Tag sym = forall (tagtyp::TagType). Tag { tagUnwrap :: Tag'' tagtyp sym }
deriving instance Show sym => Show (Tag sym)

instance Ord sym => Eq (Tag sym) where
  Tag x == Tag y = compareTag x y == EQ

instance Ord sym => Ord (Tag sym) where
  compare (Tag x) (Tag y) = compareTag x y

instance Pretty sym => Pretty (Tag sym) where
  pretty (Tag x) = pretty x
  {-# INLINE pretty #-}


-- ---------------------------------------------------------------------------
-- Decide tag types.

isSuspFunction :: Tag sym -> Bool
isSuspFunction (Tag TagSusp{}) = True
isSuspFunction (Tag TagSusp'{}) = True
isSuspFunction _ = False

isFunction :: Tag sym -> Bool
isFunction (Tag TagFunc{}) = True
isFunction (Tag TagFunc'{}) = True
isFunction _ = False


isPartialAp :: Tag sym -> Bool
isPartialAp (Tag TagPApp{}) = True
isPartialAp _ = False

isTag :: Tag sym -> Bool
isTag (Tag TagDataCons{}) = True
isTag (Tag TagPApp{}) = True
isTag (Tag TagSusp{}) = True
isTag (Tag TagSusp'{}) = True
isTag (Tag TagTypePApp{}) = True
isTag _ = False

isWHNF :: Tag sym -> Bool
isWHNF (Tag TagPApp{}) = True
isWHNF (Tag TagTypeCons{}) = True
isWHNF (Tag TagDataCons{}) = True
isWHNF (Tag TagTypePApp{}) = True
isWHNF _ = False


-- ---------------------------------------------------------------------------
-- Convert


toPartial :: forall sym. Tag sym -> Word -> Maybe (Tag sym)
toPartial (Tag (x@TagFunc{})) 0 = Just $ Tag $ TagSusp x True
toPartial (Tag (x@TagFunc{})) n = Just $ Tag $ TagPApp x n
toPartial x@(Tag TagTypeCons{}) 0 = Just x
toPartial (Tag (x@TagTypeCons{})) n = Just $ Tag $ TagTypePApp x n
toPartial (Tag (x@TagFunc'{})) 0 = Just $ Tag $ TagSusp' x True
toPartial _ _ = Nothing


toFunction :: Tag sym -> Maybe (Tag sym)
toFunction x = snd <$> tagUnfunction x


tagUnfunction :: Tag sym -> Maybe (Word, Tag sym)
tagUnfunction (Tag (TagSusp x _)) = Just (0, Tag x)
tagUnfunction (Tag (TagSusp' x _)) = Just (0,  Tag x)
tagUnfunction x@(Tag TagFunc{}) = Just (0, x)
tagUnfunction x@(Tag TagFunc'{}) = Just (0, x)
tagUnfunction (Tag (TagPApp x y)) = Just (y, Tag x)
tagUnfunction _ = Nothing


flipFunction :: Tag sym -> Maybe (Tag sym)
flipFunction (Tag (TagSusp x _)) = Just $ Tag x
flipFunction (Tag (TagSusp' x _)) = Just $ Tag x
flipFunction (Tag x@TagFunc{}) = Just $ Tag $ TagSusp x True
flipFunction (Tag x@TagFunc'{}) = Just $ Tag $ TagSusp' x True
flipFunction _ = Nothing


-- vim: ts=8 sw=2 expandtab :


