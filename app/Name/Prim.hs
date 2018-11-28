{- WARNING! Do not edit!!!
   This code is autogenerated from ../jhc-upstream/src/data/names.txt! -}

{-# Language NoOverloadedStrings, MagicHash #-}
module Name.Prim where

import Name.Internals
import StringTable.Atom
import System.IO.Unsafe

{-# NOINLINE tc_IOErrorType #-}
tc_IOErrorType = forgeName TypeConstructor (Just mod_JhcTypeHandle) "IOErrorType"
{-# NOINLINE tc_IOMode #-}
tc_IOMode = forgeName TypeConstructor (Just mod_JhcTypeHandle) "IOMode"
{-# NOINLINE tc_ACIO #-}
tc_ACIO = forgeName TypeConstructor (Just mod_JhcPrimIO) "ACIO"
{-# NOINLINE tc_Arrow #-}
tc_Arrow = forgeName TypeConstructor (Just mod_JhcPrimPrim) "->"
{-# NOINLINE tc_Bang_ #-}
tc_Bang_ = forgeName TypeConstructor (Just mod_JhcPrimRts) "Bang_"
{-# NOINLINE tc_Bool #-}
tc_Bool = forgeName TypeConstructor (Just mod_JhcPrimPrim) "Bool"
{-# NOINLINE tc_Bool_ #-}
tc_Bool_ = forgeName TypeConstructor (Just mod_JhcPrimBits) "Bool_"
{-# NOINLINE tc_Char #-}
tc_Char = forgeName TypeConstructor (Just mod_JhcTypeBasic) "Char"
{-# NOINLINE tc_IO #-}
tc_IO = forgeName TypeConstructor (Just mod_JhcPrimIO) "IO"
{-# NOINLINE tc_List #-}
tc_List = forgeName TypeConstructor (Just mod_JhcPrimPrim) "[]"
{-# NOINLINE tc_MutArray__ #-}
tc_MutArray__ = forgeName TypeConstructor (Just mod_JhcPrimArray) "MutArray_"
{-# NOINLINE tc_Ordering #-}
tc_Ordering = forgeName TypeConstructor (Just mod_JhcPrimPrim) "Ordering"
{-# NOINLINE tc_RealWorld #-}
tc_RealWorld = forgeName TypeConstructor (Just mod_JhcPrimIO) "RealWorld"
{-# NOINLINE tc_ST #-}
tc_ST = forgeName TypeConstructor (Just mod_JhcPrimIO) "ST"
{-# NOINLINE tc_State_ #-}
tc_State_ = forgeName TypeConstructor (Just mod_JhcPrimIO) "State_"
{-# NOINLINE tc_Unit #-}
tc_Unit = forgeName TypeConstructor (Just mod_JhcPrimPrim) "()"
{-# NOINLINE tc_Ratio #-}
tc_Ratio = forgeName TypeConstructor (Just mod_JhcTypeFloat) "Ratio"
{-# NOINLINE tc_Float #-}
tc_Float = forgeName TypeConstructor (Just mod_JhcTypeFloat) "Float"
{-# NOINLINE tc_Double #-}
tc_Double = forgeName TypeConstructor (Just mod_JhcTypeFloat) "Double"
{-# NOINLINE tc_Ptr #-}
tc_Ptr = forgeName TypeConstructor (Just mod_JhcTypePtr) "Ptr"
{-# NOINLINE tc_FunPtr #-}
tc_FunPtr = forgeName TypeConstructor (Just mod_JhcTypePtr) "FunPtr"
{-# NOINLINE tc_Integer #-}
tc_Integer = forgeName TypeConstructor (Just mod_JhcTypeBasic) "Integer"
{-# NOINLINE tc_Int #-}
tc_Int = forgeName TypeConstructor (Just mod_JhcTypeWord) "Int"
{-# NOINLINE tc_Bits1 #-}
tc_Bits1 = forgeName TypeConstructor (Just mod_JhcPrimBits) "Bits1_"
{-# NOINLINE tc_Bits8 #-}
tc_Bits8 = forgeName TypeConstructor (Just mod_JhcPrimBits) "Bits8_"
{-# NOINLINE tc_Bits16 #-}
tc_Bits16 = forgeName TypeConstructor (Just mod_JhcPrimBits) "Bits16_"
{-# NOINLINE tc_Bits32 #-}
tc_Bits32 = forgeName TypeConstructor (Just mod_JhcPrimBits) "Bits32_"
{-# NOINLINE tc_Bits64 #-}
tc_Bits64 = forgeName TypeConstructor (Just mod_JhcPrimBits) "Bits64_"
{-# NOINLINE tc_Bits128 #-}
tc_Bits128 = forgeName TypeConstructor (Just mod_JhcPrimBits) "Bits128_"
{-# NOINLINE tc_BitsPtr #-}
tc_BitsPtr = forgeName TypeConstructor (Just mod_JhcPrimBits) "BitsPtr_"
{-# NOINLINE tc_BitsMax #-}
tc_BitsMax = forgeName TypeConstructor (Just mod_JhcPrimBits) "BitsMax_"
{-# NOINLINE tc_Float32 #-}
tc_Float32 = forgeName TypeConstructor (Just mod_JhcPrimBits) "Float32_"
{-# NOINLINE tc_Float64 #-}
tc_Float64 = forgeName TypeConstructor (Just mod_JhcPrimBits) "Float64_"
{-# NOINLINE tc_Float80 #-}
tc_Float80 = forgeName TypeConstructor (Just mod_JhcPrimBits) "Float80_"
{-# NOINLINE tc_Float128 #-}
tc_Float128 = forgeName TypeConstructor (Just mod_JhcPrimBits) "Float128_"
{-# NOINLINE tc_Complex #-}
tc_Complex = forgeName TypeConstructor (Just mod_JhcPrimBits) "Complex_"
{-# NOINLINE tc_Addr_ #-}
tc_Addr_ = forgeName TypeConstructor (Just mod_JhcPrimBits) "Addr_"
{-# NOINLINE tc_FunAddr_ #-}
tc_FunAddr_ = forgeName TypeConstructor (Just mod_JhcPrimBits) "FunAddr_"
{-# NOINLINE tc_Char_ #-}
tc_Char_ = forgeName TypeConstructor (Just mod_JhcPrimBits) "Char_"
{-# NOINLINE dc_Boolzh #-}
dc_Boolzh = forgeName DataConstructor (Just mod_JhcPrimPrim) "Bool#"
{-# NOINLINE dc_Char #-}
dc_Char = forgeName DataConstructor (Just mod_JhcTypeBasic) "Char"
{-# NOINLINE dc_Cons #-}
dc_Cons = forgeName DataConstructor (Just mod_JhcPrimPrim) ":"
{-# NOINLINE dc_EQ #-}
dc_EQ = forgeName DataConstructor (Just mod_JhcPrimPrim) "EQ"
{-# NOINLINE dc_EmptyList #-}
dc_EmptyList = forgeName DataConstructor (Just mod_JhcPrimPrim) "[]"
{-# NOINLINE dc_False #-}
dc_False = forgeName DataConstructor (Just mod_JhcPrimPrim) "False"
{-# NOINLINE dc_GT #-}
dc_GT = forgeName DataConstructor (Just mod_JhcPrimPrim) "GT"
{-# NOINLINE dc_Int #-}
dc_Int = forgeName DataConstructor (Just mod_JhcTypeWord) "Int"
{-# NOINLINE dc_Integer #-}
dc_Integer = forgeName DataConstructor (Just mod_JhcTypeBasic) "Integer"
{-# NOINLINE dc_Just #-}
dc_Just = forgeName DataConstructor (Just mod_JhcTypeBasic) "Just"
{-# NOINLINE dc_LT #-}
dc_LT = forgeName DataConstructor (Just mod_JhcPrimPrim) "LT"
{-# NOINLINE dc_Nothing #-}
dc_Nothing = forgeName DataConstructor (Just mod_JhcTypeBasic) "Nothing"
{-# NOINLINE dc_Pair #-}
dc_Pair = forgeName DataConstructor (Just mod_JhcPrimPrim) "(,)"
{-# NOINLINE dc_Ratio #-}
dc_Ratio = forgeName DataConstructor (Just mod_JhcTypeFloat) ":%"
{-# NOINLINE dc_True #-}
dc_True = forgeName DataConstructor (Just mod_JhcPrimPrim) "True"
{-# NOINLINE dc_Unit #-}
dc_Unit = forgeName DataConstructor (Just mod_JhcPrimPrim) "()"
{-# NOINLINE dc_Word #-}
dc_Word = forgeName DataConstructor (Just mod_JhcTypeWord) "Word"
{-# NOINLINE class_Eq #-}
class_Eq = forgeName ClassName (Just mod_JhcClassOrd) "Eq"
{-# NOINLINE class_Ord #-}
class_Ord = forgeName ClassName (Just mod_JhcClassOrd) "Ord"
{-# NOINLINE class_Enum #-}
class_Enum = forgeName ClassName (Just mod_JhcEnum) "Enum"
{-# NOINLINE class_Bounded #-}
class_Bounded = forgeName ClassName (Just mod_JhcEnum) "Bounded"
{-# NOINLINE class_Show #-}
class_Show = forgeName ClassName (Just mod_JhcShow) "Show"
{-# NOINLINE class_Read #-}
class_Read = forgeName ClassName (Just mod_JhcTextRead) "Read"
{-# NOINLINE class_Ix #-}
class_Ix = forgeName ClassName (Just mod_DataIx) "Ix"
{-# NOINLINE class_Functor #-}
class_Functor = forgeName ClassName (Just mod_JhcMonad) "Functor"
{-# NOINLINE class_Monad #-}
class_Monad = forgeName ClassName (Just mod_JhcMonad) "Monad"
{-# NOINLINE class_Num #-}
class_Num = forgeName ClassName (Just mod_JhcClassNum) "Num"
{-# NOINLINE class_Real #-}
class_Real = forgeName ClassName (Just mod_JhcClassReal) "Real"
{-# NOINLINE class_Integral #-}
class_Integral = forgeName ClassName (Just mod_JhcClassReal) "Integral"
{-# NOINLINE class_Fractional #-}
class_Fractional = forgeName ClassName (Just mod_JhcClassReal) "Fractional"
{-# NOINLINE class_Floating #-}
class_Floating = forgeName ClassName (Just mod_JhcFloat) "Floating"
{-# NOINLINE class_RealFrac #-}
class_RealFrac = forgeName ClassName (Just mod_JhcFloat) "RealFrac"
{-# NOINLINE class_RealFloat #-}
class_RealFloat = forgeName ClassName (Just mod_JhcFloat) "RealFloat"
{-# NOINLINE class_Typeable #-}
class_Typeable = forgeName ClassName (Just mod_DataTypeable) "Typeable"
{-# NOINLINE class_Typeable1 #-}
class_Typeable1 = forgeName ClassName (Just mod_DataTypeable) "Typeable1"
{-# NOINLINE class_Typeable2 #-}
class_Typeable2 = forgeName ClassName (Just mod_DataTypeable) "Typeable2"
{-# NOINLINE class_Typeable3 #-}
class_Typeable3 = forgeName ClassName (Just mod_DataTypeable) "Typeable3"
{-# NOINLINE class_Foldable #-}
class_Foldable = forgeName ClassName (Just mod_DataFoldable) "Foldable"
{-# NOINLINE class_Monoid #-}
class_Monoid = forgeName ClassName (Just mod_DataMonoid) "Monoid"
{-# NOINLINE class_Traversable #-}
class_Traversable = forgeName ClassName (Just mod_DataTraversable) "Traversable"
{-# NOINLINE rt_bits8 #-}
rt_bits8 = forgeName RawType Nothing "bits8"
{-# NOINLINE rt_bits16 #-}
rt_bits16 = forgeName RawType Nothing "bits16"
{-# NOINLINE rt_bits32 #-}
rt_bits32 = forgeName RawType Nothing "bits32"
{-# NOINLINE rt_bits64 #-}
rt_bits64 = forgeName RawType Nothing "bits64"
{-# NOINLINE rt_bits128 #-}
rt_bits128 = forgeName RawType Nothing "bits128"
{-# NOINLINE rt_bool #-}
rt_bool = forgeName RawType Nothing "bool"
{-# NOINLINE rt_float32 #-}
rt_float32 = forgeName RawType Nothing "fbits32"
{-# NOINLINE rt_float64 #-}
rt_float64 = forgeName RawType Nothing "fbits64"
{-# NOINLINE rt_float80 #-}
rt_float80 = forgeName RawType Nothing "fbits80"
{-# NOINLINE rt_float128 #-}
rt_float128 = forgeName RawType Nothing "fbits128"
{-# NOINLINE rt_bits_max_ #-}
rt_bits_max_ = forgeName RawType Nothing "bits<max>"
{-# NOINLINE rt_bits_ptr_ #-}
rt_bits_ptr_ = forgeName RawType Nothing "bits<ptr>"
{-# NOINLINE v_error_ #-}
v_error_ = forgeName Val (Just mod_JhcPrimBasics) "error_"
{-# NOINLINE v_undefined #-}
v_undefined = forgeName Val (Just mod_JhcPrimBasics) "undefined"
{-# NOINLINE v_cat #-}
v_cat = forgeName Val (Just mod_JhcPrimList) "++"
{-# NOINLINE v_compose #-}
v_compose = forgeName Val (Just mod_JhcPrimBasics) "."
{-# NOINLINE v_Dot #-}
v_Dot = forgeName Val (Just mod_JhcPrimBasics) "."
{-# NOINLINE v_eqString #-}
v_eqString = forgeName Val (Just mod_JhcString) "eqString"
{-# NOINLINE v_eqUnpackedString #-}
v_eqUnpackedString = forgeName Val (Just mod_JhcString) "eqUnpackedString"
{-# NOINLINE v_error #-}
v_error = forgeName Val (Just mod_JhcIO) "error"
{-# NOINLINE v_fmap #-}
v_fmap = forgeName Val (Just mod_JhcMonad) "fmap"
{-# NOINLINE v_fmap_const #-}
v_fmap_const = forgeName Val (Just mod_JhcMonad) "<$"
{-# NOINLINE v_and #-}
v_and = forgeName Val (Just mod_JhcOrder) "&&"
{-# NOINLINE v_foldl #-}
v_foldl = forgeName Val (Just mod_JhcBasics) "foldl"
{-# NOINLINE v_drop #-}
v_drop = forgeName Val (Just mod_JhcList) "drop"
{-# NOINLINE v_concat #-}
v_concat = forgeName Val (Just mod_JhcPrimList) "concat"
{-# NOINLINE v_concatMap #-}
v_concatMap = forgeName Val (Just mod_JhcPrimList) "concatMap"
{-# NOINLINE v_filter #-}
v_filter = forgeName Val (Just mod_JhcPrimList) "filter"
{-# NOINLINE v_foldr #-}
v_foldr = forgeName Val (Just mod_JhcPrimList) "foldr"
{-# NOINLINE v_map #-}
v_map = forgeName Val (Just mod_JhcPrimList) "map"
{-# NOINLINE v_bind #-}
v_bind = forgeName Val (Just mod_JhcMonad) ">>="
{-# NOINLINE v_bind_ #-}
v_bind_ = forgeName Val (Just mod_JhcMonad) ">>"
{-# NOINLINE v_fail #-}
v_fail = forgeName Val (Just mod_JhcMonad) "fail"
{-# NOINLINE v_fromInteger #-}
v_fromInteger = forgeName Val (Just mod_JhcClassNum) "fromInteger"
{-# NOINLINE v_fromInt #-}
v_fromInt = forgeName Val (Just mod_JhcClassNum) "fromInt"
{-# NOINLINE v_sub #-}
v_sub = forgeName Val (Just mod_JhcClassNum) "-"
{-# NOINLINE v_fromRational #-}
v_fromRational = forgeName Val (Just mod_JhcClassReal) "fromRational"
{-# NOINLINE v_negate #-}
v_negate = forgeName Val (Just mod_JhcClassNum) "negate"
{-# NOINLINE v_compare #-}
v_compare = forgeName Val (Just mod_JhcClassOrd) "compare"
{-# NOINLINE v_leq #-}
v_leq = forgeName Val (Just mod_JhcClassOrd) "<="
{-# NOINLINE v_geq #-}
v_geq = forgeName Val (Just mod_JhcClassOrd) ">="
{-# NOINLINE v_lt #-}
v_lt = forgeName Val (Just mod_JhcClassOrd) "<"
{-# NOINLINE v_gt #-}
v_gt = forgeName Val (Just mod_JhcClassOrd) ">"
{-# NOINLINE v_equals #-}
v_equals = forgeName Val (Just mod_JhcClassOrd) "=="
{-# NOINLINE v_fromEnum #-}
v_fromEnum = forgeName Val (Just mod_JhcEnum) "fromEnum"
{-# NOINLINE v_toEnum #-}
v_toEnum = forgeName Val (Just mod_JhcEnum) "toEnum"
{-# NOINLINE v_enumFrom #-}
v_enumFrom = forgeName Val (Just mod_JhcEnum) "enumFrom"
{-# NOINLINE v_enumFromTo #-}
v_enumFromTo = forgeName Val (Just mod_JhcEnum) "enumFromTo"
{-# NOINLINE v_enumFromThenTo #-}
v_enumFromThenTo = forgeName Val (Just mod_JhcEnum) "enumFromThenTo"
{-# NOINLINE v_enumFromThen #-}
v_enumFromThen = forgeName Val (Just mod_JhcEnum) "enumFromThen"
{-# NOINLINE v_succ #-}
v_succ = forgeName Val (Just mod_JhcEnum) "succ"
{-# NOINLINE v_pred #-}
v_pred = forgeName Val (Just mod_JhcEnum) "pred"
{-# NOINLINE v_minBound #-}
v_minBound = forgeName Val (Just mod_JhcEnum) "minBound"
{-# NOINLINE v_maxBound #-}
v_maxBound = forgeName Val (Just mod_JhcEnum) "maxBound"
{-# NOINLINE v_showsPrec #-}
v_showsPrec = forgeName Val (Just mod_JhcShow) "showsPrec"
{-# NOINLINE v_showParen #-}
v_showParen = forgeName Val (Just mod_JhcShow) "showParen"
{-# NOINLINE v_showChar #-}
v_showChar = forgeName Val (Just mod_JhcShow) "showChar"
{-# NOINLINE v_showString #-}
v_showString = forgeName Val (Just mod_JhcShow) "showString"
{-# NOINLINE v_readsPrec #-}
v_readsPrec = forgeName Val (Just mod_JhcTextRead) "readsPrec"
{-# NOINLINE v_readParen #-}
v_readParen = forgeName Val (Just mod_JhcTextRead) "readParen"
{-# NOINLINE v_lex #-}
v_lex = forgeName Val (Just mod_JhcTextRead) "lex"
{-# NOINLINE v_range #-}
v_range = forgeName Val (Just mod_DataIx) "range"
{-# NOINLINE v_index #-}
v_index = forgeName Val (Just mod_DataIx) "index"
{-# NOINLINE v_unsafeIndex #-}
v_unsafeIndex = forgeName Val (Just mod_DataIx) "unsafeIndex"
{-# NOINLINE v_inRange #-}
v_inRange = forgeName Val (Just mod_DataIx) "inRange"
{-# NOINLINE v_runExpr #-}
v_runExpr = forgeName Val (Just mod_PreludeIO) "runExpr"
{-# NOINLINE v_runRaw #-}
v_runRaw = forgeName Val (Just mod_JhcPrimWrapper) "runRaw"
{-# NOINLINE v_runMain #-}
v_runMain = forgeName Val (Just mod_JhcIO) "runMain"
{-# NOINLINE v_runNoWrapper #-}
v_runNoWrapper = forgeName Val (Just mod_JhcPrimWrapper) "runNoWrapper"
{-# NOINLINE v_foldMap #-}
v_foldMap = forgeName Val (Just mod_DataFoldable) "foldMap"
{-# NOINLINE v_fold #-}
v_fold = forgeName Val (Just mod_DataFoldable) "fold"
{-# NOINLINE v_mempty #-}
v_mempty = forgeName Val (Just mod_DataMonoid) "mempty"
{-# NOINLINE v_mappend #-}
v_mappend = forgeName Val (Just mod_DataMonoid) "mappend"
{-# NOINLINE v_traverse #-}
v_traverse = forgeName Val (Just mod_DataTraversable) "traverse"
{-# NOINLINE v_sequenceA #-}
v_sequenceA = forgeName Val (Just mod_DataTraversable) "sequenceA"
{-# NOINLINE v_lstar #-}
v_lstar = forgeName Val (Just mod_ControlApplicative) "<*>"
{-# NOINLINE v_pure #-}
v_pure = forgeName Val (Just mod_ControlApplicative) "pure"
{-# NOINLINE v_enum_succ #-}
v_enum_succ = forgeName Val (Just mod_JhcInstPrimEnum) "enum_succ"
{-# NOINLINE v_enum_pred #-}
v_enum_pred = forgeName Val (Just mod_JhcInstPrimEnum) "enum_pred"
{-# NOINLINE v_enum_from #-}
v_enum_from = forgeName Val (Just mod_JhcInstPrimEnum) "enum_from"
{-# NOINLINE v_enum_fromTo #-}
v_enum_fromTo = forgeName Val (Just mod_JhcInstPrimEnum) "enum_fromTo"
{-# NOINLINE v_enum_fromThen #-}
v_enum_fromThen = forgeName Val (Just mod_JhcInstPrimEnum) "enum_fromThen"
{-# NOINLINE v_enum_fromThenTo #-}
v_enum_fromThenTo = forgeName Val (Just mod_JhcInstPrimEnum) "enum_fromThenTo"
{-# NOINLINE v_enum_toEnum #-}
v_enum_toEnum = forgeName Val (Just mod_JhcInstPrimEnum) "enum_toEnum"
{-# NOINLINE v_ix_index #-}
v_ix_index = forgeName Val (Just mod_JhcInstPrimEnum) "ix_index"
{-# NOINLINE v_ix_inRange #-}
v_ix_inRange = forgeName Val (Just mod_JhcInstPrimEnum) "ix_inRange"
{-# NOINLINE v_ix_range #-}
v_ix_range = forgeName Val (Just mod_JhcInstPrimEnum) "ix_range"
{-# NOINLINE s_Star #-}
s_Star = forgeName SortName Nothing "*"
{-# NOINLINE s_Hash #-}
s_Hash = forgeName SortName Nothing "#"
{-# NOINLINE s_Bang #-}
s_Bang = forgeName SortName Nothing "!"
{-# NOINLINE s_Tuple #-}
s_Tuple = forgeName SortName Nothing "(#)"
{-# NOINLINE s_Quest #-}
s_Quest = forgeName SortName Nothing "?"
{-# NOINLINE s_QuestQuest #-}
s_QuestQuest = forgeName SortName Nothing "??"
{-# NOINLINE s_StarBang #-}
s_StarBang = forgeName SortName Nothing "*!"
{-# NOINLINE s_HashHash #-}
s_HashHash = forgeName SortName Nothing "##"
{-# NOINLINE s_StarStar #-}
s_StarStar = forgeName SortName Nothing "**"
{-# NOINLINE vu_At #-}
vu_At = forgeName Val Nothing "@"
{-# NOINLINE vu_Bang #-}
vu_Bang = forgeName Val Nothing "!"
{-# NOINLINE vu_Dot #-}
vu_Dot = forgeName Val Nothing "."
{-# NOINLINE vu_Hash #-}
vu_Hash = forgeName Val Nothing "#"
{-# NOINLINE vu_Minus #-}
vu_Minus = forgeName Val Nothing "-"
{-# NOINLINE vu_Quest #-}
vu_Quest = forgeName Val Nothing "?"
{-# NOINLINE vu_QuestQuest #-}
vu_QuestQuest = forgeName Val Nothing "??"
{-# NOINLINE vu_Star #-}
vu_Star = forgeName Val Nothing "*"
{-# NOINLINE vu_StarBang #-}
vu_StarBang = forgeName Val Nothing "*!"
{-# NOINLINE vu_Twiddle #-}
vu_Twiddle = forgeName Val Nothing "~"
{-# NOINLINE vu_alias #-}
vu_alias = forgeName Val Nothing "alias"
{-# NOINLINE vu_as #-}
vu_as = forgeName Val Nothing "as"
{-# NOINLINE vu_closed #-}
vu_closed = forgeName Val Nothing "closed"
{-# NOINLINE vu_derive #-}
vu_derive = forgeName Val Nothing "derive"
{-# NOINLINE vu_exists #-}
vu_exists = forgeName Val Nothing "exists"
{-# NOINLINE vu_family #-}
vu_family = forgeName Val Nothing "family"
{-# NOINLINE vu_forall #-}
vu_forall = forgeName Val Nothing "forall"
{-# NOINLINE vu_hiding #-}
vu_hiding = forgeName Val Nothing "hiding"
{-# NOINLINE vu_kind #-}
vu_kind = forgeName Val Nothing "kind"
{-# NOINLINE vu_qualified #-}
vu_qualified = forgeName Val Nothing "qualified"
{-# NOINLINE vu_main #-}
vu_main = forgeName Val Nothing "main"
{-# NOINLINE vu_import #-}
vu_import = forgeName Val Nothing "import"
{-# NOINLINE vu_sub #-}
vu_sub = forgeName Val Nothing "-"
{-# NOINLINE vu_enumFrom #-}
vu_enumFrom = forgeName Val Nothing "enumFrom"
{-# NOINLINE vu_enumFromThen #-}
vu_enumFromThen = forgeName Val Nothing "enumFromThen"
{-# NOINLINE vu_enumFromThenTo #-}
vu_enumFromThenTo = forgeName Val Nothing "enumFromThenTo"
{-# NOINLINE vu_enumFromTo #-}
vu_enumFromTo = forgeName Val Nothing "enumFromTo"
{-# NOINLINE u_At #-}
u_At = forgeName UnknownType Nothing "@"
{-# NOINLINE u_Bang #-}
u_Bang = forgeName UnknownType Nothing "!"
{-# NOINLINE u_Dot #-}
u_Dot = forgeName UnknownType Nothing "."
{-# NOINLINE u_DotDot #-}
u_DotDot = forgeName UnknownType Nothing ".."
{-# NOINLINE u_Hash #-}
u_Hash = forgeName UnknownType Nothing "#"
{-# NOINLINE u_Minus #-}
u_Minus = forgeName UnknownType Nothing "-"
{-# NOINLINE u_Quest #-}
u_Quest = forgeName UnknownType Nothing "?"
{-# NOINLINE u_QuestQuest #-}
u_QuestQuest = forgeName UnknownType Nothing "??"
{-# NOINLINE u_Star #-}
u_Star = forgeName UnknownType Nothing "*"
{-# NOINLINE u_StarBang #-}
u_StarBang = forgeName UnknownType Nothing "*!"
{-# NOINLINE u_Twiddle #-}
u_Twiddle = forgeName UnknownType Nothing "~"
{-# NOINLINE u_alias #-}
u_alias = forgeName UnknownType Nothing "alias"
{-# NOINLINE u_as #-}
u_as = forgeName UnknownType Nothing "as"
{-# NOINLINE u_closed #-}
u_closed = forgeName UnknownType Nothing "closed"
{-# NOINLINE u_derive #-}
u_derive = forgeName UnknownType Nothing "derive"
{-# NOINLINE u_exists #-}
u_exists = forgeName UnknownType Nothing "exists"
{-# NOINLINE u_family #-}
u_family = forgeName UnknownType Nothing "family"
{-# NOINLINE u_forall #-}
u_forall = forgeName UnknownType Nothing "forall"
{-# NOINLINE u_hiding #-}
u_hiding = forgeName UnknownType Nothing "hiding"
{-# NOINLINE u_kind #-}
u_kind = forgeName UnknownType Nothing "kind"
{-# NOINLINE u_qualified #-}
u_qualified = forgeName UnknownType Nothing "qualified"
{-# NOINLINE u_instance #-}
u_instance = forgeName UnknownType Nothing "instance"
{-# NOINLINE u_import #-}
u_import = forgeName UnknownType Nothing "import"
{-# NOINLINE u_placeholder #-}
u_placeholder = forgeName UnknownType Nothing "placeholder"

{-# NOINLINE mod_ControlApplicative #-}
mod_ControlApplicative = Module (unsafePerformIO $ addrToAtom_ "Control.Applicative"# 19# )
{-# NOINLINE mod_DataFoldable #-}
mod_DataFoldable = Module (unsafePerformIO $ addrToAtom_ "Data.Foldable"# 13# )
{-# NOINLINE mod_DataIx #-}
mod_DataIx = Module (unsafePerformIO $ addrToAtom_ "Data.Ix"# 7# )
{-# NOINLINE mod_DataMonoid #-}
mod_DataMonoid = Module (unsafePerformIO $ addrToAtom_ "Data.Monoid"# 11# )
{-# NOINLINE mod_DataTraversable #-}
mod_DataTraversable = Module (unsafePerformIO $ addrToAtom_ "Data.Traversable"# 16# )
{-# NOINLINE mod_DataTypeable #-}
mod_DataTypeable = Module (unsafePerformIO $ addrToAtom_ "Data.Typeable"# 13# )
{-# NOINLINE mod_JhcBasics #-}
mod_JhcBasics = Module (unsafePerformIO $ addrToAtom_ "Jhc.Basics"# 10# )
{-# NOINLINE mod_JhcClassNum #-}
mod_JhcClassNum = Module (unsafePerformIO $ addrToAtom_ "Jhc.Class.Num"# 13# )
{-# NOINLINE mod_JhcClassOrd #-}
mod_JhcClassOrd = Module (unsafePerformIO $ addrToAtom_ "Jhc.Class.Ord"# 13# )
{-# NOINLINE mod_JhcClassReal #-}
mod_JhcClassReal = Module (unsafePerformIO $ addrToAtom_ "Jhc.Class.Real"# 14# )
{-# NOINLINE mod_JhcEnum #-}
mod_JhcEnum = Module (unsafePerformIO $ addrToAtom_ "Jhc.Enum"# 8# )
{-# NOINLINE mod_JhcFloat #-}
mod_JhcFloat = Module (unsafePerformIO $ addrToAtom_ "Jhc.Float"# 9# )
{-# NOINLINE mod_JhcIO #-}
mod_JhcIO = Module (unsafePerformIO $ addrToAtom_ "Jhc.IO"# 6# )
{-# NOINLINE mod_JhcInstPrimEnum #-}
mod_JhcInstPrimEnum = Module (unsafePerformIO $ addrToAtom_ "Jhc.Inst.PrimEnum"# 17# )
{-# NOINLINE mod_JhcList #-}
mod_JhcList = Module (unsafePerformIO $ addrToAtom_ "Jhc.List"# 8# )
{-# NOINLINE mod_JhcMonad #-}
mod_JhcMonad = Module (unsafePerformIO $ addrToAtom_ "Jhc.Monad"# 9# )
{-# NOINLINE mod_JhcOrder #-}
mod_JhcOrder = Module (unsafePerformIO $ addrToAtom_ "Jhc.Order"# 9# )
{-# NOINLINE mod_JhcPrimArray #-}
mod_JhcPrimArray = Module (unsafePerformIO $ addrToAtom_ "Jhc.Prim.Array"# 14# )
{-# NOINLINE mod_JhcPrimBasics #-}
mod_JhcPrimBasics = Module (unsafePerformIO $ addrToAtom_ "Jhc.Prim.Basics"# 15# )
{-# NOINLINE mod_JhcPrimBits #-}
mod_JhcPrimBits = Module (unsafePerformIO $ addrToAtom_ "Jhc.Prim.Bits"# 13# )
{-# NOINLINE mod_JhcPrimIO #-}
mod_JhcPrimIO = Module (unsafePerformIO $ addrToAtom_ "Jhc.Prim.IO"# 11# )
{-# NOINLINE mod_JhcPrimList #-}
mod_JhcPrimList = Module (unsafePerformIO $ addrToAtom_ "Jhc.Prim.List"# 13# )
{-# NOINLINE mod_JhcPrimPrim #-}
mod_JhcPrimPrim = Module (unsafePerformIO $ addrToAtom_ "Jhc.Prim.Prim"# 13# )
{-# NOINLINE mod_JhcPrimRts #-}
mod_JhcPrimRts = Module (unsafePerformIO $ addrToAtom_ "Jhc.Prim.Rts"# 12# )
{-# NOINLINE mod_JhcPrimWrapper #-}
mod_JhcPrimWrapper = Module (unsafePerformIO $ addrToAtom_ "Jhc.Prim.Wrapper"# 16# )
{-# NOINLINE mod_JhcShow #-}
mod_JhcShow = Module (unsafePerformIO $ addrToAtom_ "Jhc.Show"# 8# )
{-# NOINLINE mod_JhcString #-}
mod_JhcString = Module (unsafePerformIO $ addrToAtom_ "Jhc.String"# 10# )
{-# NOINLINE mod_JhcTextRead #-}
mod_JhcTextRead = Module (unsafePerformIO $ addrToAtom_ "Jhc.Text.Read"# 13# )
{-# NOINLINE mod_JhcTypeBasic #-}
mod_JhcTypeBasic = Module (unsafePerformIO $ addrToAtom_ "Jhc.Type.Basic"# 14# )
{-# NOINLINE mod_JhcTypeC #-}
mod_JhcTypeC = Module (unsafePerformIO $ addrToAtom_ "Jhc.Type.C"# 10# )
{-# NOINLINE mod_JhcTypeFloat #-}
mod_JhcTypeFloat = Module (unsafePerformIO $ addrToAtom_ "Jhc.Type.Float"# 14# )
{-# NOINLINE mod_JhcTypeHandle #-}
mod_JhcTypeHandle = Module (unsafePerformIO $ addrToAtom_ "Jhc.Type.Handle"# 15# )
{-# NOINLINE mod_JhcTypePtr #-}
mod_JhcTypePtr = Module (unsafePerformIO $ addrToAtom_ "Jhc.Type.Ptr"# 12# )
{-# NOINLINE mod_JhcTypeWord #-}
mod_JhcTypeWord = Module (unsafePerformIO $ addrToAtom_ "Jhc.Type.Word"# 13# )
{-# NOINLINE mod_Main #-}
mod_Main = Module (unsafePerformIO $ addrToAtom_ "Main"# 4# )
{-# NOINLINE mod_Main_ #-}
mod_Main_ = Module (unsafePerformIO $ addrToAtom_ "Main@"# 5# )
{-# NOINLINE mod_Prelude #-}
mod_Prelude = Module (unsafePerformIO $ addrToAtom_ "Prelude"# 7# )
{-# NOINLINE mod_PreludeIO #-}
mod_PreludeIO = Module (unsafePerformIO $ addrToAtom_ "Prelude.IO"# 10# )
{-# NOINLINE mod_Prim_ #-}
mod_Prim_ = Module (unsafePerformIO $ addrToAtom_ "Prim@"# 5# )
{-# NOINLINE mod_Wild_ #-}
mod_Wild_ = Module (unsafePerformIO $ addrToAtom_ "Wild@"# 5# )
{-# NOINLINE mod_unknown #-}
mod_unknown = Module (unsafePerformIO $ addrToAtom_ "unknown"# 7# )
{-# NOINLINE qdc_Cons #-}
qdc_Cons = quoteName dc_Cons
{-# NOINLINE qdc_EQ #-}
qdc_EQ = quoteName dc_EQ
{-# NOINLINE qdc_False #-}
qdc_False = quoteName dc_False
{-# NOINLINE qdc_GT #-}
qdc_GT = quoteName dc_GT
{-# NOINLINE qdc_Just #-}
qdc_Just = quoteName dc_Just
{-# NOINLINE qdc_LT #-}
qdc_LT = quoteName dc_LT
{-# NOINLINE qdc_Nothing #-}
qdc_Nothing = quoteName dc_Nothing
{-# NOINLINE qdc_Pair #-}
qdc_Pair = quoteName dc_Pair
{-# NOINLINE qdc_True #-}
qdc_True = quoteName dc_True
{-# NOINLINE qdc_Unit #-}
qdc_Unit = quoteName dc_Unit
{-# NOINLINE qv_Dot #-}
qv_Dot = quoteName v_Dot
{-# NOINLINE qv_and #-}
qv_and = quoteName v_and
{-# NOINLINE qv_compare #-}
qv_compare = quoteName v_compare
{-# NOINLINE qv_equals #-}
qv_equals = quoteName v_equals
{-# NOINLINE qv_geq #-}
qv_geq = quoteName v_geq
{-# NOINLINE qv_showParen #-}
qv_showParen = quoteName v_showParen
{-# NOINLINE qv_showString #-}
qv_showString = quoteName v_showString
{-# NOINLINE qv_showsPrec #-}
qv_showsPrec = quoteName v_showsPrec