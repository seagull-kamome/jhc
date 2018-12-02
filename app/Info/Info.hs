module Info.Info(
    Entry(..),
    entryType,
    --
    Info(..),
    singleton, empty,
    insert, deleteTyp, delete,
    lookupTyp, member
    ) where

import Data.Dynamic (Dynamic, dynTypeRep, fromDynamic, toDyn)
import Data.Typeable (Typeable, TypeRep, typeOf, TypeRep)
import qualified Data.Map as Map

import qualified Info.Properties as Prop


-- ---------------------------------------------------------------------------
-- 

data Entry = Entry { entryThing :: Dynamic, entryString :: String }

instance Eq Entry where a == b = entryType a == entryType b
instance Show Entry where show = entryString
instance Ord Entry where compare a b = compare (entryType a) (entryType b)

entryType :: Entry -> TypeRep
entryType = dynTypeRep . entryThing



-- ---------------------------------------------------------------------------
-- 

data Info = Info { infoProperties :: !Prop.PropertySet, infoMap :: Map.Map TypeRep Entry }
    deriving(Typeable)
instance Show Info where show Info {.. } = show (Map.toList infoMap)
instance Semigroup Info where
  (Info ap as) <> (Info bp bs) = Info (ap <> bp) (Map.union as bs)
instance Monoid Info where mempty = Info Prop.empty Map.empty


-- | Construction

singleton :: (Show a,Typeable a) => a -> Info
singleton x = insert x empty

empty :: Info
empty = mempty



-- | Insert / Delete

insert :: (Show a,Typeable a) => a -> Info -> Info
insert newx y = y { infoMap = Map.insert typ newEntry (infoMap y) } where
    typ = typeOf newx
    newEntry = Entry { entryThing = toDyn newx, entryString = show newx }

deleteTyp :: TypeRep -> Info -> Info
deleteTyp typ (Info a mp) = Info a (Map.delete typ mp)

delete :: forall a proxy. Typeable a => proxy a -> Info -> Info
delete _ = deleteTyp $ typeOf (undefined :: a)



-- | Lookup

lookupTyp :: forall a. Typeable a => Info -> Maybe a
lookupTyp (Info _ mp) = Map.lookup typ mp >>= fromDynamic . entryThing where
    typ = typeOf (undefined :: a)

member :: forall a proxy. (Typeable a) => proxy a -> Info -> Bool
member _ Info { .. } = Map.member (typeOf (undefined :: a)) infoMap




