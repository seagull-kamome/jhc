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
  IdKinds(..),
  idKindIsConstructor,
  --
  ModuleName(..), toModuleName,
  --
  Name(..), toName, nameParts,
  nameIsConstructor,
  --
  Id(..), IdSet, IdMap,
  idKind, idIsConstructor
  ) where

import Data.Set as SET
import Data.Map.Strict as MAP
import GHC.Generics

import qualified Data.Text as T
import Data.Interned (intern, unintern)
import Data.Interned.Text
import qualified Data.Binary as BIN


-- ---------------------------------------------------------------------------

data IdKindUniv = KindLevelId | TypeLevelId | TermLevelId | UnknownLevelId
  deriving (Eq, Show)

data IdKinds
  = UnknownType
  | TypeConstructor | DataConstructor
  | TypeVal | TermVal
  | ModuleName | ClassName | SortName | FieldLabel
  | RawType
  deriving (Enum, Eq, Show, Generic)
instance BIN.Binary IdKinds

idKindIsConstructor :: IdKinds -> Bool
idKindIsConstructor TypeConstructor = True
idKindIsConstructor DataConstructor = True
idKindIsConstructor _ = False


-- ---------------------------------------------------------------------------

data ModuleName
  = UnqualifiedModuleName { moduleBaseName :: !InternedText }
  | QualifiedModuleName { moduleParentName :: !ModuleName
                        , moduleBaseName :: !InternedText }
  deriving (Show, Eq)
instance BIN.Binary ModuleName where
  put (UnqualifiedModuleName x) = BIN.putWord8 0 >> BIN.put (unintern x)
  put (QualifiedModuleName x y) = BIN.putWord8 1 >> BIN.put x >> BIN.put (unintern y)
  get = BIN.getWord8 >>= \case
    0 -> UnqualifiedModuleName <$> (intern <$> BIN.get)
    1 -> QualifiedModuleName <$> BIN.get <*> (intern <$> BIN.get)
    _ -> undefined
--instance IsString ModuleName where
--  fromString xs = case span (/= '.') xs of
--    (x1, "") -> UnqualifiedName $ intern x1
--    (x1, x2) -> QualifiedName (intern x1) $ fromString x2
--  {-# INLINE fromString #-}
--  toString (UnqualifiedName x) = T.unpack $ unintern x
--  toString (QualifiedName x y) = T.unpack (unintern x) ++ toString y
--  {-# INLINE toString #-}


toModuleName :: Maybe ModuleName -> T.Text -> ModuleName
toModuleName Nothing = UnqualifiedModuleName . intern
toModuleName (Just x) = QualifiedModuleName x . intern



data Name = UnqualifiedName { nameKind:: !IdKinds, nameBase:: !InternedText }
          | QualifiedName { nameKind:: !IdKinds
                          , nameModule:: !ModuleName
                          , nameBase :: !InternedText }
  deriving (Show, Eq)
instance BIN.Binary Name where
  put (UnqualifiedName x y) = BIN.putWord8 0 >> BIN.put x >> BIN.put (unintern y)
  put (QualifiedName x y z) = BIN.putWord8 1 >> BIN.put x >> BIN.put y >> BIN.put (unintern z)
  get = BIN.getWord8 >>= \case
    0 -> UnqualifiedName <$> BIN.get <*> (intern <$> BIN.get)
    1 -> QualifiedName <$> BIN.get <*> BIN.get <*> (intern <$> BIN.get)
    _ -> undefined

toName :: Maybe ModuleName -> IdKinds ->T.Text -> Name
toName Nothing k = UnqualifiedName k . intern
toName (Just x) k = QualifiedName k x . intern


nameParts :: Name -> (IdKinds, Maybe ModuleName, Name)
nameParts x@(UnqualifiedName k _) = (k, Nothing, x)
nameParts (QualifiedName k m b) = (k, Just m, UnqualifiedName k b)


nameIsConstructor :: Name -> Bool
nameIsConstructor x = idKindIsConstructor $ nameKind x

-- ---------------------------------------------------------------------------

data Id = NameId !Name | Id !IdKinds !Int
  deriving (Show, Eq)
instance BIN.Binary Id where
  put (NameId x) = BIN.putWord8 0 >> BIN.put x
  put (Id x y) = BIN.putWord8 1 >> BIN.put x >> BIN.put y
  get = BIN.getWord8 >>= \case
    0 -> NameId <$> BIN.get
    1 -> Id <$> BIN.get <*> BIN.get
    _ -> undefined


type IdSet = SET.Set Id
type IdMap a = MAP.Map Id a


idKind :: Id  -> IdKinds
idKind (NameId x) = nameKind x
idKind (Id x _) = x

idIsConstructor :: Id -> Bool
idIsConstructor (NameId x) = nameIsConstructor x
idIsConstructor (Id x _) = idKindIsConstructor x


-- ---------------------------------------------------------------------------


