module Info.Properties (
  Property (..), readProp, PropertySet,
  --
  EBS.empty,
  EBS.insert, EBS.delete,
  EBS.member, EBS.notMember
  ) where

import qualified Data.EnumSet.EnumSmallBitSet as EBS

-- ---------------------------------------------------------------------------

data Property
  = PROP_INLINE | PROP_MULTISPECIALIZE | PROP_NOINLINE | PROP_SRCLOC_ANNOTATE
  | PROP_SUPERINLINE | PROP_NOETA | PROP_HOT | PROP_COLD | PROP_CYCLIC
  | PROP_EXPORTED | PROP_INSTANCE | PROP_JOINPOINT | PROP_METHOD
  | PROP_ONESHOT | PROP_PLACEHOLDER | PROP_RULEBINDER | PROP_SCRUTINIZED
  | PROP_SPECIALIZATION | PROP_SRCLOC_ANNOTATE_FUN | PROP_SUPERSPECIALIZE
  | PROP_UNSHARED | PROP_WHNF | PROP_WORKER | PROP_WRAPPER | PROP_HASRULE
    deriving(Eq, Ord, Enum, Bounded)
type PropertySet = EBS.EnumBitSet32 Property

readProp :: Monad m => String -> m Property
readProp "INLINE" = return PROP_INLINE
readProp "MULTISPECIALIZE" = return PROP_MULTISPECIALIZE
readProp "NOINLINE" = return PROP_NOINLINE
readProp "SRCLOC_ANNOTATE" = return PROP_SRCLOC_ANNOTATE
readProp "SUPERINLINE" = return PROP_SUPERINLINE
readProp "NOETA" = return PROP_NOETA
readProp "HOT" = return PROP_HOT
readProp "COLD" = return PROP_COLD
readProp p = fail $ "Invalid Property: " ++ p

-- ---------------------------------------------------------------------------


