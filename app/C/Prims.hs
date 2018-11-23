{-# LANGUAGE OverloadedStrings #-}
module C.Prims where

import Data.Binary
import Data.Monoid (Monoid(..))
import Data.Typeable
import qualified Data.Set as Set
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS -- utf8-string
import GHC.Exts
import GHC.Generics (Generic)
import Text.PrettyPrint.ANSI.Leijen (Pretty(pretty), text, char, parens, tupled)

import StringTable.Atom
import qualified Cmm.Op as Op



data CallConv = CCall | StdCall | CApi | Primitive | DotNet
    deriving(Eq,Ord,Show,Generic)
instance Binary CallConv



data Safety = Safe | Unsafe deriving(Eq,Ord,Show, Generic)
instance Binary Safety



newtype ExtType = ExtType BS.ByteString deriving(IsString,Eq,Ord, Generic)
instance Binary ExtType
instance Show ExtType where
    show (ExtType p) = BS.toString p




newtype Requires = Requires (Set.Set (CallConv, BS.ByteString))
    deriving(Eq,Ord,Semigroup,Monoid, Generic)
instance Binary Requires
instance Show Requires where
    show (Requires s) = show (Set.toList s)



data DotNetPrim = DotNetField | DotNetCtor | DotNetMethod
    deriving(Typeable, Eq, Ord, Show, Generic)
instance Binary DotNetPrim



data Prim =
    PrimPrim Atom -- Special primitive implemented in the compiler somehow.
    | CConst {
        primRequires :: Requires,
        primConst    :: !BS.ByteString
        }  -- C code which evaluates to a constant
    | Func {
        primRequires :: Requires,
        funcName     :: !BS.ByteString,
        primArgTypes :: [ExtType],
        primRetType  :: ExtType,
        primRetArgs  :: [ExtType],
        primSafety   :: Safety
        }   -- function call with C calling convention
    | IFunc {
        primRequires :: Requires,
        primArgTypes :: [ExtType],
        primRetType  :: ExtType
        } -- indirect function call with C calling convention
    | AddrOf {
        primRequires :: Requires,
        primConst    :: !BS.ByteString -- address of linker name
        }
    | Peek { primArgTy :: Op.Ty }  -- read value from memory
    | Poke { primArgTy :: Op.Ty }  -- write value to memory
    | PrimTypeInfo {
        primArgTy    :: Op.Ty,
        primRetTy    :: Op.Ty,
        primTypeInfo :: !PrimTypeInfo
        }
    | PrimString !BS.ByteString  -- address of a raw string. encoded in utf8.
    | PrimDotNet {
        primStatic     :: !Bool,
        primDotNet     :: !DotNetPrim,
        primIOLike     :: !Bool,
        primAssembly   :: !BS.ByteString,
        primDotNetName :: !BS.ByteString
        }
    | Op {
        primCOp   :: Op.Op Op.Ty,
        primRetTy :: Op.Ty
        }
    deriving(Typeable, Eq, Ord, Show, Generic)
instance Binary Prim

primReqs :: Prim -> Requires
primReqs p = f p where
    f CConst {} = primRequires p
    f Func {}   = primRequires p
    f IFunc {}  = primRequires p
    f AddrOf {} = primRequires p
    f _         = mempty



data PrimTypeInfo = PrimSizeOf | PrimMaxBound | PrimMinBound | PrimAlignmentOf | PrimUMaxBound
    deriving(Typeable, Eq, Ord, Show, Generic)
instance Binary PrimTypeInfo

primStaticTypeInfo :: Op.Ty -> PrimTypeInfo -> Maybe Integer
primStaticTypeInfo (Op.TyBits (Op.Bits b) _) w = Just ans where
    bits = toInteger b
    ans = case w of
        PrimSizeOf      -> bits `div` 8
        PrimAlignmentOf -> bits `div` 8
        PrimMinBound    -> negate $ 2^(bits - 1)
        PrimMaxBound    -> 2^(bits - 1) - 1
        PrimUMaxBound   -> 2^bits - 1
primStaticTypeInfo _ _ = Nothing



-- | These primitives may safely be duplicated without affecting performance or
-- correctness too adversly. either because they are cheap to begin with, or
-- will be recombined in a later pass.

primIsCheap :: Prim -> Bool
primIsCheap AddrOf {}       = True
primIsCheap CConst {}       = True
primIsCheap PrimString {}   = True
primIsCheap PrimTypeInfo {} = True
primIsCheap Op { primCOp = op }  = Op.isCheap op
primIsCheap _               = False

-- | whether a primitive represents a constant expression (assuming all its arguments are constant)
-- TODO needs grin support
primIsConstant :: Prim -> Bool
primIsConstant CConst {}       = True
primIsConstant AddrOf {}       = True
primIsConstant PrimString {}   = True
primIsConstant PrimTypeInfo {} = True
primIsConstant Op { primCOp = op } = Op.isEagerSafe op
primIsConstant _ = False

-- | whether a primitive can be eagarly evaluated.
-- TODO needs grin support
primEagerSafe :: Prim -> Bool
primEagerSafe CConst {}       = True
primEagerSafe PrimString {}   = True
primEagerSafe AddrOf {}       = True
primEagerSafe PrimTypeInfo {} = True
primEagerSafe Op { primCOp = op } = Op.isEagerSafe op
primEagerSafe _ = False

primPrim :: ToAtom a => a -> Prim
primPrim = PrimPrim . toAtom


instance Pretty ExtType where pretty = text . show
instance Pretty Prim where
    pretty (PrimPrim t)   = text (fromAtom t)
    pretty (CConst _ s)   = parens (text $ BS.toString s)
    pretty Func { .. }    = parens (text $ show primRetType) <> text (BS.toString funcName) <> tupled (map pretty primArgTypes)
    pretty IFunc { .. }   = parens (text $ show primRetType) <> parens (char '*') <> tupled (map pretty primArgTypes)
    pretty (AddrOf _ s)   = char '&' <> text (BS.toString s)
    pretty (PrimString s) = text (show s) <> char '#'
    pretty (Peek t)       = char '*' <> text (show t)
    pretty (Poke t)       = char '=' <> text (show t)
    pretty Op { primCOp = Op.BinOp bo ta tb, primRetTy = rt } | rt == ta && rt == tb = parens (pretty rt) <> text (show bo)
    pretty Op { primCOp = Op.UnOp bo ta, primRetTy = rt } | rt == ta = parens (pretty rt) <> text (show bo)
    pretty Op { primCOp = op, primRetTy = rt } = parens (pretty rt) <> pretty op
    pretty PrimDotNet { primDotNet = dn,  primDotNetName = nn} = parens (text (BS.toString nn))
    pretty PrimTypeInfo { primArgTy = at, primTypeInfo = PrimSizeOf } = text "sizeof" <> parens (text $ show at)
    pretty PrimTypeInfo { primArgTy = at, primTypeInfo = PrimAlignmentOf } = text "alignmentof" <> parens (text $ show at)
    pretty PrimTypeInfo { primArgTy = at, primTypeInfo = PrimMaxBound } = text "max" <> parens (text $ show at)
    pretty PrimTypeInfo { primArgTy = at, primTypeInfo = PrimUMaxBound } = text "umax" <> parens (text $ show at)
    pretty PrimTypeInfo { primArgTy = at, primTypeInfo = PrimMinBound } = text "min" <> parens (text $ show at)
instance Pretty Op.Ty where pretty = text . show
instance Show v => Pretty (Op.Op v) where pretty = text . show


parseDotNetFFI :: Monad m => String -> m Prim
parseDotNetFFI s = ans where
    init = PrimDotNet { primIOLike = False, primStatic = False, primDotNet = DotNetField, primAssembly = BS.fromString "", primDotNetName = BS.fromString "" }
    ans = case words s of
        ("static":rs) -> f rs init { primStatic = True }
        rs -> f rs init
    f ("field":rs) dn = g dn { primDotNet = DotNetField } rs
    f ("ctor":rs) dn = g dn { primDotNet = DotNetCtor } rs
    f ("method":rs) dn = g dn { primDotNet = DotNetMethod } rs
    f _ _ = fail "invalid .NET ffi specification"
    g dn ['[':rs] | (as,']':nm) <- span (/= ']') rs = return dn { primAssembly = BS.fromString as, primDotNetName = BS.fromString nm }
    g dn [n] = return dn { primDotNetName = BS.fromString n }
    g _ _ = fail "invalid .NET ffi specification"

