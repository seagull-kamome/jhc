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
{-# OverloadedStrings #-}
module Name.Prim (
  valLstar, valPure,
  --
  valfoldl,
  --
  valEnumSucc, valEnumPred, valEnumFrom, valEnumFromTo,
  valEnumFromThen, valEnumFromThenTo, valEnumToEnum,
  --
  valIxIndex, valIxInRange, valIxRange,
  --
  valAnd,
  --
  valDrop,
  --
  tcIOErrorType, tcIOMode,
  --
  tcChar, tcInteger, dcChar, dcInteger, dcJust, dcNothing,
  --
  tcRatio, tcFloat, tcDouble, dcRatio,
  --
  tcPtr, tcFunPtr,
  --
  tcInt, dcInt, dcWord,
  --
  valError_, valUndefined, valCompose,
  --
  tcACIO, tcIO, tcRealWorld, tcST, tcState_,
  --
  tcArrow, tcBang_, tcBool, tcList,tcOrdering, tcUnit,
  dcBoolzh, dcCons, dcEQ, dcEmptyList, dcFalse, dcGT, dcLT, dcPair, dcTrue, dcUnit,
  --
  tcBool_, tcBits1, tcBits8, tcBits16, tcBits32, tcBits64, tcBits128,
  tcBitsPtr, tcBitsMax, tcFloat32, tcFloat64, tcFloat80, tcFloat128,
  tcAddr_, tcFunAddr_, tcChar,
  --
  valCat, valConcat, valConcatMap, valFilter, valFoldr, valMap,
  --
  tcMutArray__,
  --
  valRunRaw, valRunNoWrapper,
  --
  classEq, classOrd, valCompare, valLEQ, valGEQ, valLT, valGT, valEQ,
  --
  classShow, valShowsPrec, valShowParen, valShowChar, valShowString,
  --
  classRead, valReadsPrec, valReadParen, valLex,
  --
  classIx, valRange, valIndex, valUnsafeIndex, valInRange,
  --
  valError, valRunMain,
  --
  classFunctor, classMonad, valFmap, valFmapConst, valBind, valBind_, valFail,
  --
  classNum, valFromInteger, valFromInt, valSub, valNagate,
  --
  classReal, classIntegral, classFractional, valFromRational,
  --
  classFloating, classRealFrac, classRealFloat,
  --
  valEqString, valEqUnpackedString,
  --
  classTypeable, classTypeable1, classTypeable2, classTypeable3,
  --
  classFoldable, valFoldMap, valFold,
  --
  classMonoid, valMempty, valMappend,
  --
  classTraversable, valTraverse, valSequenceA,
  --
  valRunExpr
  ) where

import qualified Data.Text as T
import Name.Id
import Data.Interned (intern, unintern)
import Data.Interned.Text


forgeName :: IdKinds -> String -> Name
forgeName k name = case f [] name of
    [x] -> UnqualifiedName k x
    (x : xs) -> QualifiedName k (g xs) x
  where
    f :: [InternedText] -> String -> [InternedText]
    f xs ys = case span (/= '.') ys of
      (y1, "") -> intern (T.pack y1) : xs
      (y1, ".") -> intern (T.pack ".") : intern (T.pack y1) : xs
      (y1, y2) -> f (intern (T.pack y1) : xs) y2
    g [] = undefined
    g [x] = UnqualifiedModuleName x
    g (x:xs) = QualifiedModuleName (g xs) x
{-# INLINE forgeName #-}


--
-- module Control.Applicative
--
valLstar, valPure :: Name
valLstar = forgeName TermVal "Control.Applicative.<*>"
valPure = forgeName TermVal "Control.Applicative.pure"


--
-- module Jhc.Basics
--
valfoldl :: Name
valfoldl = forgeName TermVal "Jhc.Basics.foldl"


--
-- module Jhc.Inst.PrimEnum
--
valEnumSucc, valEnumPred, valEnumFrom, valEnumFromTo :: Name
valEnumFromThen, valEnumFromThenTo, valEnumToEnum :: Name
valEnumSucc = forgeName TermVal "Jhc.Inst.PrimEnum.enum_succ"
valEnumPred = forgeName TermVal "Jhc.Inst.PrimEnum.enum_pred"
valEnumFrom = forgeName TermVal "Jhc.Inst.PrimEnum.enum_from"
valEnumFromTo = forgeName TermVal "Jhc.Inst.PrimEnum.enum_fromTo"
valEnumFromThen = forgeName TermVal "Jhc.Inst.PrimEnum.enum_fromThen"
valEnumFromThenTo = forgeName TermVal "Jhc.Inst.PrimEnum.enum_fromTHenTo"
valEnumToEnum = forgeName TermVal "Jhc.Inst.PrimEnum.enun_toENum"


valIxIndex, valIxInRange, valIxRange :: Name
valIxIndex = forgeName TermVal "Jhc.Inst.PrimEnum.ix_index"
valIxInRange = forgeName TermVal "Jhc.Inst.PrimEnum.ix_inRange"
valIxRange = forgeName TermVal "Jhc.Inst.PrimEnum.ix_range"



--
-- module Jhc.Order
--
valAnd :: Name
valAnd = forgeName TermVal "Jhc.Order.&&"



--
-- module Jhc.List
--
valDrop :: Name
valDrop = forgeName TermVal "Jhc.List.drop"


--
-- module Jhc.Type.Handle
--
tcIOErrorType, tcIOMode :: Name
tcIOErrorType = forgeName TypeConstructor "Jhc.Type.IOErrorType"
tcIOMode = forgeName TypeConstructor "Jhc.Type.IOMode"

--
-- module Jhc.Type.Basic
--
tcChar, tcInteger, dcChar, dcInteger, dcJust, dcNothing :: Name
tcChar = forgeName TypeConstructor "Jhc.Type.Basic.Char"
tcInteger = forgeName TypeConstructor "Jhc.Type.Basic.Integer"
dcChar = forgeName DataConstructor "Jhc.Type.Basic.Char"
dcInteger = forgeName DataConstructor "Jhc.Type.Basic.Integer"
dcJust = forgeName DataConstructor "Jhc.Type.Basic.Just"
dcNothing = forgeName DataConstructor "Jhc.Type.Basic.Nothing"

--
-- module Jhc.Type.Float
--
tcRatio, tcFloat, tcDouble, dcRatio :: Name
tcRatio = forgeName TypeConstructor "Jhc.Type.Float.Ratio"
tcFloat = forgeName TypeConstructor "Jhc.Type.Float.Float"
tcDouble = forgeName TypeConstructor "Jhc.Type.Float.Double"
dcRatio = forgeName DataConstructor "Jhc.Type.Float.:%"

--
-- module Jhc.Type.Ptr
--
tcPtr, tcFunPtr :: Name
tcPtr = forgeName TypeConstructor "Jhc.Type.Ptr.Ptr"
tcFunPtr = forgeName TypeConstructor "Jhc.Type.Ptr.FunPtr"

--
-- module Jhc.Type.Word
--
tcInt, dcInt, dcWord :: Name
tcInt = forgeName TypeConstructor "Jhc.Type.Word.Int"
dcInt = forgeName DataConstructor "Jhc.Type.Word.Int"
dcWord = forgeName DataConstructor "Jhc.Type.Word.Word"


--
-- module Jhc.Prim.Basics
--
valError_, valUndefined, valCompose :: Name
valError_ = forgeName TermVal "Jhc.Prim.Basics.error_"
valUndefined = forgeName TermVal "Jhc.Prim.Basics.undefined"
valCompose = forgeName TermVal "Jhc.Prim.Basics.."



--
-- module Jhc.Prim.IO
--
tcACIO, tcIO, tcRealWorld, tcST, tcState_ :: Name
tcACIO = forgeName TypeConstructor "Jhc.Prim.IO.ACIO"
tcIO = forgeName TypeConstructor "Jhc.Prim.IO.IO"
tcRealWorld = forgeName TypeConstructor "Jhc.Prim.IO.RealWorld"
tcST = forgeName TypeConstructor "Jhc.Prim.IO.ST"
tcState_ = forgeName TypeConstructor "Jhc.Prim.IO.State"



--
-- module Jhc.Prim.Prim
--
tcArrow, tcBang_, tcBool, tcList,tcOrdering, tcUnit :: Name
tcArrow = forgeName TypeConstructor "Jhc.Prim.Prim.->"
tcBang_ = forgeName TypeConstructor "Jhc.Prim.Prim.Bang_"
tcBool = forgeName TypeConstructor "Jhc.Prim.Prim.Bool"
tcList = forgeName TypeConstructor "Jhc.Prim.Prim.[]"
tcOrdering = forgeName TypeConstructor "Jhc.Prim.Prim.Ordering"
tcUnit = forgeName TypeConstructor "Jhc.Prim.Prim.()"

dcBoolzh, dcCons, dcEQ, dcEmptyList, dcFalse, dcGT, dcLT, dcPair, dcTrue, dcUnit :: Name
dcBoolzh = forgeName DataConstructor "Jhc.Prim.Prim.Bool#"
dcCons = forgeName DataConstructor "Jhc.Prim.Prim.:"
dcEQ = forgeName DataConstructor "Jhc.Prim.Prim.EQ"
dcEmptyList = forgeName DataConstructor "Jhc.Prim.Prim.[]"
dcFalse = forgeName DataConstructor "Jhc.Prim.Prim.False"
dcGT = forgeName DataConstructor "Jhc.Prim.Prim.GT"
dcLT = forgeName DataConstructor "Jhc.Prim.Prim.LT"
dcPair = forgeName DataConstructor "Jhc.Prim.Prim.(,)"
dcTrue = forgeName DataConstructor "Jhc.Prim.Prim.True"
dcUnit = forgeName DataConstructor "Jhc.Prim.Prim.()"


--
-- module Jhc.Prim.Bits
--
tcBool_, tcBits1, tcBits8, tcBits16, tcBits32, tcBits64, tcBits128 :: Name
tcBitsPtr, tcBitsMax, tcFloat32, tcFloat64, tcFloat80, tcFloat128 :: Name
tcAddr_, tcFunAddr_, tcChar_:: Name
tcBool_ = forgeName TypeConstructor "Jhc.Prim.Bits.Bool_"
tcBits1 = forgeName TypeConstructor "Jhc.Prim.Bits.Bits1_"
tcBits8 = forgeName TypeConstructor "Jhc.Prim.Bits.Bits8_"
tcBits16 = forgeName TypeConstructor "Jhc.Prim.Bits.Bits16_"
tcBits32 = forgeName TypeConstructor "Jhc.Prim.Bits.Bits32_"
tcBits64 = forgeName TypeConstructor "Jhc.Prim.Bits.Bits64_"
tcBits128 = forgeName TypeConstructor "Jhc.Prim.Bits.Bits128_"
tcBitsPtr = forgeName TypeConstructor "Jhc.Prim.Bits.BitsPtr_"
tcBitsMax = forgeName TypeConstructor "Jhc.Prim.Bits.BitsMax_"
tcFloat32 = forgeName TypeConstructor "Jhc.Prim.Bits.Fload32_"
tcFloat64 = forgeName TypeConstructor "Jhc.Prim.Bits.Fload64_"
tcFloat80 = forgeName TypeConstructor "Jhc.Prim.Bits.FLoad80_"
tcFloat128 = forgeName TypeConstructor "Jhc.Prim.Bits.Float128_"
tcComplex = forgeName TypeConstructor "Jhc.Prim.Bits.Complex_"
tcAddr_ = forgeName TypeConstructor "Jhc.Prim.Bits.Addr_"
tcFunAddr_ = forgeName TypeConstructor "Jhc.Prim.Bits.FunAddr_"
tcChar_ = forgeName TypeConstructor "Jhc.Prim.Bits.Char_"


--
-- module Jhc.Prim.List
--
valCat, valConcat, valConcatMap, valFilter, valFoldr, valMap :: Name
valCat = forgeName TermVal "Jhc.Prim.List.++"
valConcat = forgeName TermVal "Jhc.Prim.List.concat"
valConcatMap = forgeName TermVal "Jhc.Prim.List.concatMap"
valFilter = forgeName TermVal "Jhc.Prim.List.filter"
valFoldr = forgeName TermVal "Jhc.Prim.List.foldr"
valMap = forgeName TermVal "Jhc.Prim.List.map"


--
-- module Jhc.Prim.Array
--
tcMutArray__ :: Name
tcMutArray__ = forgeName TypeConstructor "Jhc.Prim.MutArray"


--
-- module Jhc.Prim.Wrapper
--
valRunRaw, valRunNoWrapper :: Name
valRunRaw = forgeName TermVal "Jhc.Prim.Wrapper.runRaw"
valRunNoWrapper = forgeName TermVal "Jhc.Prim.Wrapper.runNoWrapper"


--
-- module Jhc.Class.Ord
--
classEq, classOrd, valCompare, valLEQ, valGEQ, valLT, valGT, valEQ :: Name
classEq = forgeName ClassName "Jhc.Class.Ord.Eq"
classOrd = forgeName ClassName "Jhc.Class.Ord.Ord"
valCompare = forgeName TermVal "Jhc.Class.Ord.compare"
valLEQ = forgeName TermVal "Jhc.Class.Ord.<="
valGEQ = forgeName TermVal "Jhc.Class.Ord.>="
valLT = forgeName TermVal "Jhc.Class.Ord.<"
valGT = forgeName TermVal "Jhc.Class.Ord.>"
valEQ = forgeName TermVal "Jhc.Class.Ord.=="


#if 0
--
-- module Jhc.Enum
--
classEnum = forgeName ClassName "Jhc.Enum.Enum"
classBounded = forgeName ClassName "Jhc.Enum.Bounded"
valFromEnum = forgeName TermVal "Jhc.Enum.fromEnum"
valToEnum = forgeName TermVal "Jhc.Enum.toEnum"
valEnumFrom = forgeName TermVal "Jhc.Enum.enumFrom"
valEnumFromTo = forgeName TermVal "Jhc.Enum.enumFromTo"
valEnumFromThenTo = forgeName TermVal "Jhc.Enum.enumFromThenTo"
valEnumFromThen = forgeName TermVal "Jhc.Enum.enumFromTHen"
valSucc = forgeName TermVal "Jhc.Enum.succ"
valPred = forgeName TermVal "Jhc.Enum.pred"
valMinBound = forgeName TermVal "Jhc.Enum.minBOund"
valMaxBound = forgeName TermVal "Jhc.Enum.maxBound"
#endif


--
-- module Jhc.Show
--
classShow, valShowsPrec, valShowParen, valShowChar, valShowString :: Name
classShow = forgeName ClassName "Jhc.Show.Show"
valShowsPrec = forgeName TermVal "Jhc.Show.showPrec"
valShowParen = forgeName TermVal "Jhc.Show.showParen"
valShowChar = forgeName TermVal "Jhc.Show.showChar"
valShowString = forgeName TermVal "Jhc.Show.showString"


--
-- module Jhc.Text.Read
--
classRead, valReadsPrec, valReadParen, valLex :: Name
classRead = forgeName ClassName "Jhc.Text.Read.Read"
valReadsPrec = forgeName TermVal "Jhc.Text.Read.readsPrec"
valReadParen = forgeName TermVal "Jhc.Text.Read.readParen"
valLex = forgeName TermVal "Jhc.Text.Read.lex"



--
-- module Data.Ix
--
classIx, valRange, valIndex, valUnsafeIndex, valInRange :: Name
classIx = forgeName ClassName "Data.Ix.Ix"
valRange = forgeName TermVal "Data.Ix.range"
valIndex = forgeName TermVal "Data.Ix.index"
valUnsafeIndex = forgeName TermVal "Data.Ix.unsafeIndex"
valInRange = forgeName TermVal "Data.Ix.inRange"


--
-- module Jhc.IO
--
valError, valRunMain :: Name
valError = forgeName TermVal "Jhc.IO.error"
valRunMain = forgeName TermVal "Jhc.IO.runMain"




--
-- module Jhc.Monad
--
classFunctor, classMonad, valFmap, valFmapConst, valBind, valBind_, valFail :: Name
classFunctor = forgeName ClassName "Jhc.Monad.Functor"
classMonad = forgeName ClassName "Jhc.Monad.Monad"
valFmap = forgeName TermVal "Jhc.Monad.fmap"
valFmapConst = forgeName TermVal "Jhc.Monad.<$"
valBind = forgeName TermVal "Jhc.Monad.>>="
valBind_ = forgeName TermVal "Jhc.Monad.>>"
valFail = forgeName TermVal "Jhc.Monad.fail"


--
-- module Jhc.Class.Num
--
classNum, valFromInteger, valFromInt, valSub, valNagate :: Name
classNum = forgeName ClassName "Jhc.Class.Num"
valFromInteger = forgeName TermVal "Jhc.Class.Num.fromInteger"
valFromInt = forgeName TermVal "Jhc.Class.Num.fromInt"
valSub = forgeName TermVal "Jhc.Class.Num.-"
valNagate = forgeName TermVal "Jhc.Class.Num.nagate"



--
-- module Jhc.Class.Real
--
classReal, classIntegral, classFractional, valFromRational :: Name
classReal = forgeName ClassName "Jhc.Class.Real.Real"
classIntegral = forgeName ClassName "Jhc.Class.Real.Integral"
classFractional = forgeName ClassName "Jhc.Class.Real.Fractional"
valFromRational = forgeName TermVal "Jhc.CLass.Real.fromRational"


--
-- module Jhc.Float
--
classFloating, classRealFrac, classRealFloat :: Name
classFloating = forgeName ClassName "Jhc.Float"
classRealFrac = forgeName ClassName "Jhc.RealFrac"
classRealFloat = forgeName ClassName "Jhc.RealFloat"


--
-- module Jhc.String
--
valEqString, valEqUnpackedString :: Name
valEqString = forgeName TermVal "Jhc.String.eqString"
valEqUnpackedString = forgeName TermVal "Jhc.String.eqUnpackedString"


--
-- module Data.Typeable
--
classTypeable, classTypeable1, classTypeable2, classTypeable3 :: Name
classTypeable = forgeName ClassName "Data.Typeable.Typeable"
classTypeable1 = forgeName ClassName "Data.Typeable.Typeable1"
classTypeable2 = forgeName ClassName "Data.Typeable.Typeable2"
classTypeable3 = forgeName ClassName "Data.Typeable.Typeable3"


--
-- module Data.Foldable
--
classFoldable, valFoldMap, valFold :: Name
classFoldable = forgeName ClassName "Data.Foldable"
valFoldMap = forgeName TermVal "Data.Foldable.foldMap"
valFold = forgeName TermVal "Data.Foldable.fold"




--
-- module Data.Monoid
--
classMonoid, valMempty, valMappend :: Name
classMonoid = forgeName ClassName "Data.Monoid"
valMempty = forgeName TermVal "Data.Monoid.mempty"
valMappend = forgeName TermVal "Data.Monoid.mappend"



--
-- module Data.Traversable
--
classTraversable, valTraverse, valSequenceA :: Name
classTraversable = forgeName ClassName "Data.Traversable.Traversable"
valTraverse = forgeName TermVal "Data.Traversable.traverse"
valSequenceA = forgeName TermVal "Data.Traversable.sequenceA"


--
-- module Prelude.IO
--
valRunExpr :: Name
valRunExpr = forgeName TermVal "Prelude.IO.runExpr"



--
--
--
rawtypbits8 = forgeName RawType "bits8"
rawtypbits16 = forgeName RawType "bits16"
rawtypbits32 = forgeName RawType "bits32"
rawtypbits64 = forgeName RawType "bits64"
rawtypbits128 = forgeName RawType "bits128"
rawtypbool = forgeName RawType "bool"
rawtypfloat32 = forgeName RawType "fbits32"
rawtypfloat64 = forgeName RawType "fbits64"
rawtypfloat80 = forgeName RawType "fbits80"
rawtypfloat128 = forgeName RawType "fbits128"
rawtypbitsortmax_ = forgeName RawType "bits<max>"
rawtypbitsortptr_ = forgeName RawType "bits<ptr>"
sortStar = forgeName SortName "*"
sortHash = forgeName SortName "#"
sortBang = forgeName SortName "!"
sortTuple = forgeName SortName "(#)"
sortQuest = forgeName SortName "?"
sortQuestQuest = forgeName SortName "??"
sortStarBang = forgeName SortName "*!"
sortHashHash = forgeName SortName "##"
sortStarStar = forgeName SortName "**"
valAt = forgeName TermVal "@"
valBang = forgeName TermVal "!"
valDot = forgeName TermVal "."
valHash = forgeName TermVal "#"
valMinus = forgeName TermVal "-"
valQuest = forgeName TermVal "?"
valQuestQuest = forgeName TermVal "??"
valStar = forgeName TermVal "*"
valStarBang = forgeName TermVal "*!"
valTwiddle = forgeName TermVal "~"
valalias = forgeName TermVal "alias"
valas = forgeName TermVal "as"
valclosed = forgeName TermVal "closed"
valderive = forgeName TermVal "derive"
valexists = forgeName TermVal "exists"
valfamily = forgeName TermVal "family"
valforall = forgeName TermVal "forall"
valhiding = forgeName TermVal "hiding"
valkind = forgeName TermVal "kind"
valqualified = forgeName TermVal "qualified"
valmain = forgeName TermVal "main"
valimport = forgeName TermVal "import"
valsub = forgeName TermVal "-"
valenumFrom = forgeName TermVal "enumFrom"
valenumFromThen = forgeName TermVal "enumFromThen"
valenumFromThenTo = forgeName TermVal "enumFromThenTo"
valenumFromTo = forgeName TermVal "enumFromTo"
unkownAt = forgeName UnknownType "@"
unkownBang = forgeName UnknownType "!"
unkownDot = forgeName UnknownType "."
unkownDotDot = forgeName UnknownType ".."
unkownHash = forgeName UnknownType "#"
unkownMinus = forgeName UnknownType "-"
unkownQuest = forgeName UnknownType "?"
unkownQuestQuest = forgeName UnknownType "??"
unkownStar = forgeName UnknownType "*"
unkownStarBang = forgeName UnknownType "*!"
unkownTwiddle = forgeName UnknownType "~"
unkownalias = forgeName UnknownType "alias"
unkownas = forgeName UnknownType "as"
unkownclosed = forgeName UnknownType "closed"
unkownderive = forgeName UnknownType "derive"
unkownexists = forgeName UnknownType "exists"
unkownfamily = forgeName UnknownType "family"
unkownforall = forgeName UnknownType "forall"
unkownhiding = forgeName UnknownType "hiding"
unkownkind = forgeName UnknownType "kind"
unkownqualified = forgeName UnknownType "qualified"
unkowninstance = forgeName UnknownType "instance"
unkownimport = forgeName UnknownType "import"
unkownplaceholder = forgeName UnknownType "placeholder"

--mod_JhcPrimRts = Module (unsafePerformIO $ addrToAtom_ "Jhc.Prim.Rts"# 12# )
--mod_JhcTypeC = Module (unsafePerformIO $ addrToAtom_ "Jhc.Type.C"# 10# )
--mod_Main = Module (unsafePerformIO $ addrToAtom_ "Main"# 4# )
--mod_Main_ = Module (unsafePerformIO $ addrToAtom_ "Main@"# 5# )
--mod_Prelude = Module (unsafePerformIO $ addrToAtom_ "Prelude"# 7# )
--mod_Prim_ = Module (unsafePerformIO $ addrToAtom_ "Prim@"# 5# )
--mod_Wild_ = Module (unsafePerformIO $ addrToAtom_ "Wild@"# 5# )
--mod_unknown = Module (unsafePerformIO $ addrToAtom_ "unknown"# 7# )
#if 0
qdcCons = quoteName dc_Cons
qdcEQ = quoteName dc_EQ
qdcFalse = quoteName dc_False
qdcGT = quoteName dc_GT
qdcJust = quoteName dc_Just
qdcLT = quoteName dc_LT
qdcNothing = quoteName dc_Nothing
qdcPair = quoteName dc_Pair
qdcTrue = quoteName dc_True
qdcUnit = quoteName dc_Unit
qvalDot = quoteName valDot
qvaland = quoteName valand
qvalcompare = quoteName valcompare
qvalequals = quoteName valequals
qvalgeq = quoteName valgeq
qvalshowParen = quoteName valshowParen
qvalshowString = quoteName valshowString
qvalshowsPrec = quoteName valshowsPrec
#endif

-- vim: ts=8 sw=2 expandtab :

