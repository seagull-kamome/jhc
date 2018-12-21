module Language.Grin.AST.Type (
  Callable, Typ''(..), Typ(..),
  TypeThunk(..)
  ) where


-- ---------------------------------------------------------------------------
-- | Type Universe of GRIN

data Callable = Continuation | Function | Closure | LocalFunction | Primitive'
  deriving(Eq,Ord,Show)


data Typ'' primtypes typ
  = TypPtr !typ    -- ^ pointer
  | TypNode       -- ^ a whole node
  | TypINode      -- ^ a whole possibly indirect node
  | TypAttr !typ !typ  -- ^ attach an attribute to a type
  | TypAnd !typ !typ   -- ^ boolean conjunction of types
  | TypOr !typ !typ   -- ^ boolean disjunction of types
  | TypPrim !primtypes  -- ^ a basic type
  | TypUnit        -- ^ type of Unit
  | TypCall !Callable ![typ] ![typ]  -- ^ something call,jump, or cut-to-able
  | TypRegion      -- ^ a region
  | TypGcContext   -- ^ the context for garbage collection
  | TypRegister !typ -- ^ a register contains a mutable value, the register itself cannot be addressed,
  --   hence they may not be returned from functions or passed as arguments.
  | TypComplex !typ    -- ^ A complex version of a basic type
  | TypVector !Word !typ  -- ^ A vector of a basic type
  | TypUnknown   -- ^ an unknown possibly undefined type, All of these must be eliminated by code generation
  deriving (Eq, Ord)

newtype Typ primtypes = Typ { unwrap :: Typ'' primetypes (Typ primtypes) }



-- ---------------------------------------------------------------------------
-- Type thunk and environment.

data TypeThunk'' sym typ
  = TypeNotThunk   -- ^ not the thunk
  | TypeApp !(Maybe typ) !sym -- ^ can be applied to (possibly) an argument, and what results
  | TypeSusp !sym -- ^ can be evaluated and calls what function
  deriving (Show, Eq)

type TypeThunk sym primtypes = TypeThunk'' sym (Typ primtypes)




data TypeOfType sym typ = TypeOfType {
    typSlots :: ![typ],
    typReturn :: ![typ],
    typThunk :: !(TypeThunk sym typ),
    typSiblings :: !(Maybe [sym)]
  }

newtype TypeEnv'' sym typ = TypeEnv ( fromTypeEnv :: Map.Map sym (TypeOfType'' sym typ) }
  deriving (Semigroup, Monoid)

newtype TypeEnv sym primtypes = TypeEnv { unwrap :: TypeEnv'' sym (Typ primtypes) }
  deriving (Semigroup, Monoid)




-- ---------------------------------------------------------------------------
-- Construction

emptyTypeOfType :: TypeOfType'' sym typ
emptyTypeOfType = { [], [], TypeNotThunk, Nothing }


