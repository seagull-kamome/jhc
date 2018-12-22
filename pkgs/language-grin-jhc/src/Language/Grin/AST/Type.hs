module Language.Grin.AST.Type (
  Callable, Typ(..), TypeThunk(..), TypeOfType(..), TypeEnv(..)
  ) where

import qualified Data.Default -- data-default
import Text.PrettyPrint.ANSI.Leijen


-- ---------------------------------------------------------------------------
-- | Type Universe of GRIN

data Callable = Continuation | Function | Closure | LocalFunction | Primitive'
  deriving (Show, Eq, Ord)


data Typ primtypes
  = TypPtr !(Typ primtypes)    -- ^ pointer
  | TypNode       -- ^ a whole node
  | TypINode      -- ^ a whole possibly indirect node
  | TypAttr !(Typ primtypes) !(Typ primtypes) -- ^ attach an attribute to a type
  | TypAnd !(Typ primtypes) !(Typ primtypes)  -- ^ boolean conjunction of types
  | TypOr !(Typ primtypes) !(Typ primtypes)  -- ^ boolean disjunction of types
  | TypPrim !primtypes  -- ^ a basic type
  | TypUnit        -- ^ type of Unit
  | TypCall !Callable ![Typ primtypes] ![Typ primtypes]  -- ^ something call,jump, or cut-to-able
  | TypRegion      -- ^ a region
  | TypGcContext   -- ^ the context for garbage collection
  | TypRegister !(Typ primtypes) -- ^ a register contains a mutable value, the register itself cannot be addressed,
  --   hence they may not be returned from functions or passed as arguments.
  | TypComplex !(Typ primtypes)    -- ^ A complex version of a basic type
  | TypVector !Word !(Typ primtypes)  -- ^ A vector of a basic type
  | TypUnknown   -- ^ an unknown possibly undefined type, All of these must be eliminated by code generation
  deriving (Show, Eq, Ord)


instance Pretty primtypes => Pretty (Typ primtypes) where
  pretty = \case
    TypComplex t -> "Complex" <+> pretty t
    TypVector v t -> pretty t <> char '*' <> pretty v
    TypAttr t1 t2 -> pretty t1 <> char '#' <> pretty t2
    TypAnd t1 t2 -> pretty t1 <> "&&" <> pretty t2
    TypOr t1 t2 -> pretty t1 <> "||" <> pretty t2
    TypNode -> "N"
    TypINode -> "I"
    TypPtr t -> char '&' <> pretty t
    TypUnit -> "()"
    TypRegion -> "<"
    TypGCContext -> "GC"
    TypRegister t -> char 'r' <> pretty t
    TypCall c args rt -> pretty c <> tupled (map pretty args) <+> "->" <+> pretty rt
    TypUnknown -> char '?'
    _ -> "BADTYPE"




-- ---------------------------------------------------------------------------
-- Type thunk and environment.

data TypeThunk sym primtypes
  = TypeNotThunk   -- ^ not the thunk
  | TypeApp !(Maybe (Typ primtypes)) !sym -- ^ can be applied to (possibly) an argument, and what results
  | TypeSusp !sym -- ^ can be evaluated and calls what function
  deriving (Show, Eq)




data TypeOfType sym primtypes = TypeOfType {
    typSlots :: ![Typ primtypes],
    typReturn :: ![Typ primtypes],
    typThunk :: !(TypeThunk sym primtypes),
    typSiblings :: !(Maybe [sym])
  }

instance Data.Default.Default TypeOfType where
  dft = TypeOfType [] [] TypeNotThunk Nothing



-- ---------------------------------------------------------------------------
-- Construction

emptyTypeOfType :: TypeOfType'' sym typ
emptyTypeOfType = TypeOfType []  [] TypeNotThunk Nothing


