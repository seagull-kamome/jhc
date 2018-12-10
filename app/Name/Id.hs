{-
Copyright Hattori, Hiroki (c) 2018

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Hattori, Hiroki nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
module Name.Id (
  Name, AnyName(..), KindedName(..),
  nameIdKind, nameKindSing, toUnknownName, toName,
  forgeName, nameParts,
  --
  Id, AnyId(..), KindedId(..),
  IdSet, IdMap, AnyIdSet, AnyIdMap, KindedIdSet, KindedIdMap,
  KnownIdKind(..),
  idKindSing, toUnknownId, toId,
  FreshIdT, MonadFreshId(..), runFreshIdT
  ) where

import Control.Monad.Trans
import Control.Monad.State.Strict
import Data.Set as SET
import Data.Map.Strict as MAP
-- import GHC.TypeLits
-- import GHC.Exts (IsString(..))
import GHC.Generics
-- import Data.Proxy

import qualified Data.Text as T
import Data.Interned (intern)
import Data.Interned.Text
import qualified Data.Binary as BIN

-- import Control.Monad.Fresh

-- ---------------------------------------------------------------------------

data IdKinds
  = Unknown
  | TypeConstructor | DataConstructor
  | TypeVal | TermVal
  | ModuleName | ClassName | SortName | FieldLabel
  | RawType
  | QuotedName
  deriving (Show, Generic)
instance BIN.Binary IdKinds

class KnownIdKind (k :: IdKinds) where toIdKind :: proxy k -> IdKinds
instance KnownIdKind 'TypeConstructor where toIdKind _ = TypeConstructor
instance KnownIdKind 'DataConstructor where toIdKind _ = DataConstructor
instance KnownIdKind 'TypeVal where toIdKind _ = TypeVal
instance KnownIdKind 'TermVal where toIdKind _ = TermVal
instance KnownIdKind 'ModuleName where toIdKind _ = ModuleName
instance KnownIdKind 'ClassName where toIdKind _ = ClassName
instance KnownIdKind 'SortName where toIdKind _ = SortName
instance KnownIdKind 'FieldLabel where toIdKind _ = FieldLabel
instance KnownIdKind 'RawType where toIdKind _ = RawType
instance KnownIdKind 'QuotedName where toIdKind _ = QuotedName


data SingK (k :: IdKinds) where
  SingUnknown :: SingK 'Unknown
  SingTypeConstructor :: SingK 'TypeConstructor
  SingDataConstructor :: SingK 'DataConstructor
  SingTypeVal :: SingK 'TypeVal
  SingTermVal :: SingK 'TermVal
  SingModuleName :: SingK 'ModuleName
  SingClassName :: SingK 'ClassName
  SingSortName :: SingK 'SortName
  SingFieldLabel :: SingK 'FieldLabel
  SingRawType :: SingK 'RawType
  SingQuotedName :: SingK 'QuotedName
deriving instance Show (SingK k)
deriving instance Eq (SingK k)


-- ---------------------------------------------------------------------------

data Name (k :: IdKinds) where
  UnqualifiedName' :: !InternedText -> Name 'Unknown
  UnqualifiedName :: KnownIdKind k => !(SingK k) -> !InternedText -> Name k
  QualifiedName' :: Name 'ModuleName -> !InternedText -> Name 'Unknown
  QualifiedName :: KnownIdKind k => !(SingK k) -> !(Name 'ModuleName) -> !InternedText -> Name k
deriving instance Show (Name k)
deriving instance Eq (Name k)

newtype AnyName = AnyName { fromAnyName :: forall k. Name k }
deriving instance Show AnyName
deriving instance Eq AnyName

newtype KindedName = KindedName { fromKindedName :: forall k. KnownIdKind k => Name k }
-- deriving instance Show KindedName
-- deriving instance Eq KindedName

nameIdKind :: Name (k :: IdKinds) -> IdKinds
nameIdKind (UnqualifiedName' _) = Unknown
nameIdKind (UnqualifiedName sing _) = toIdKind sing
nameIdKind (QualifiedName' _ _) = Unknown
nameIdKind (QualifiedName sing _ _) = toIdKind sing


nameKindSing :: Name (k :: IdKinds) -> SingK k
nameKindSing (UnqualifiedName' _) = SingUnknown
nameKindSing (UnqualifiedName sing _) = sing
nameKindSing (QualifiedName' _ _) = SingUnknown
nameKindSing (QualifiedName sing _ _) = sing


toUnknownName :: T.Text -> Name 'Unknown
toUnknownName = UnqualifiedName' . intern


toName :: KnownIdKind k => SingK k ->T.Text -> Name k
toName sing = UnqualifiedName sing . intern


forgeName :: KnownIdKind k => SingK k -> Name 'ModuleName -> T.Text -> Name k
--forgeName SingUnknown Nothing = UnqualifiedName' . intern
--forgeName SingUnknown (Just x) = QualifiedName' x . intern
-- forgeName sing Nothing = UnqualifiedName sing . intern
forgeName sing x = QualifiedName sing x . intern


nameParts :: Name k -> (SingK k, Maybe (Name 'ModuleName), Name k)
nameParts x@(UnqualifiedName' _) = (SingUnknown, Nothing, x)
nameParts x@(UnqualifiedName sing _) = (sing, Nothing, x)
nameParts (QualifiedName' m name) = (SingUnknown, Just m, UnqualifiedName' name)
nameParts (QualifiedName sing m name) = (sing, Just m, UnqualifiedName sing name)


-- ---------------------------------------------------------------------------

data Id (k :: IdKinds) where
  AtomId ::  Name k -> Id k
  Id' :: !Int -> Id 'Unknown
  Id :: KnownIdKind k => !(SingK k) -> !Int -> Id k
deriving instance Show (Id k)
deriving instance Eq (Id k)

newtype AnyId = AnyId { fromAnyId :: forall (k :: IdKinds). Id k }
-- deriving instance Show AnyId
deriving instance Eq AnyId

newtype KindedId = KindedId { fromKindedId :: forall (k :: IdKinds). KnownIdKind k => Id k }
-- deriving instance Show KindedId
-- deriving instance Eq KindedId

type AnyIdSet = SET.Set AnyId
type KindedIdSet = SET.Set KindedId
type IdSet k = SET.Set (Id k)

type AnyIdMap a = MAP.Map AnyId a
type KindedIdMap a = MAP.Map KindedId a
type IdMap k a = MAP.Map (Id k) a



-- ---------------------------------------------------------------------------
#if 0
instance Eq (Id (k :: IdKinds)) where
  (AtomId' x) == (AtomId' y) = x == y
  (AtomId' x) == (AtomId SingUnknown y) = x == y
  (AtomId SingUnknown x) == (AtomId' y) = x == y
  (AtomId _ x) == (AtomId _ y) = x == y
  (Id' x) == (Id' y) = x == y
  (Id' x) == (Id SingUnknown y) = x == y
  (Id SingUnknown x) == (Id' y) = x == y
  (Id _ x) == (Id _ y) = x == y
  _ == _ = False
#endif


-- ---------------------------------------------------------------------------

idKindSing :: forall (k :: IdKinds). Id k  -> SingK k
-- idKindSing (AtomId' _) = SingUnknown
idKindSing (AtomId name) = nameKindSing name
idKindSing (Id' _) = SingUnknown
idKindSing (Id sing _) = sing


toUnknownId :: T.Text -> Id 'Unknown
toUnknownId = AtomId . UnqualifiedName' . intern
{-# INLINE toUnknownId #-}



toId :: KnownIdKind k => SingK k -> T.Text -> Id k
toId sing = AtomId . UnqualifiedName sing . intern
{-# INLINE toId #-}

-- freshUnknownId :: MonadFresh Int m _=> m (Id Unknown)
-- freshId :: MonadFresh Int m => SingK k -> m (Id k)


-- ---------------------------------------------------------------------------

newtype FreshIdT m a = FreshIdT (StateT Int m a)
  deriving (Functor, Applicative, Monad)
instance MonadTrans FreshIdT where lift = FreshIdT . lift
instance MonadIO m => MonadIO (FreshIdT m) where liftIO = lift.liftIO

class MonadFreshId m where
  freshUnknonwId :: m (Id 'Unknown)
  freshId :: KnownIdKind k => SingK k -> m (Id k)
instance Monad m => MonadFreshId (FreshIdT m) where
  freshUnknonwId = FreshIdT $ do { x <- get; put (x + 1); return $ Id' x }
  freshId k = FreshIdT $ do { x <- get; put (x + 1); return $ Id k x }


runFreshIdT :: Monad m => Int -> FreshIdT m a -> m a
runFreshIdT x (FreshIdT y) = evalStateT y x



-- ---------------------------------------------------------------------------


