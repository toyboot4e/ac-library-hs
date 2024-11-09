{-# LANGUAGE TypeFamilies #-}

-- | For compatibility. FIXME: Remove with latest vector
module Util (DoNotUnboxLazy (..)) where

import Data.Coerce (coerce)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Mutable qualified as VM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

-- | Newtype which allows to derive unbox instances for type @a@ which
-- is normally a "boxed" type. The newtype does not alter the strictness
-- semantics of the underlying type and inherits the laizness of said type.
-- For a strict newtype wrapper, see 'DoNotUnboxStrict'.
--
-- 'DoNotUnboxLazy' is intended to be unsed in conjunction with the newtype 'As'
-- and the type class 'IsoUnbox'. Here's an example which uses the following
-- explicit 'IsoUnbox' instance:
--
--
-- >>> :set -XTypeFamilies -XStandaloneDeriving -XDerivingVia
-- >>> :set -XMultiParamTypeClasses -XTypeOperators -XFlexibleInstances
-- >>> import qualified Data.Vector.Unboxed         as VU
-- >>> import qualified Data.Vector.Unboxed.Mutable as VUM
-- >>> import qualified Data.Vector.Generic         as VG
-- >>> import qualified Data.Vector.Generic.Mutable as VGM
-- >>> :{
-- >>> data Foo a = Foo Int a
-- >>>   deriving (Eq, Ord, Show)
-- >>> instance VU.IsoUnbox (Foo a) (Int, VU.DoNotUnboxLazy a) where
-- >>>   toURepr (Foo i a) = (i, VU.DoNotUnboxLazy a)
-- >>>   fromURepr (i, VU.DoNotUnboxLazy a) = Foo i a
-- >>>   {-# INLINE toURepr #-}
-- >>>   {-# INLINE fromURepr #-}
-- >>> newtype instance VU.MVector s (Foo a) = MV_Foo (VU.MVector s (Int, VU.DoNotUnboxLazy a))
-- >>> newtype instance VU.Vector    (Foo a) = V_Foo  (VU.Vector    (Int, VU.DoNotUnboxLazy a))
-- >>> deriving via (Foo a `VU.As` (Int, VU.DoNotUnboxLazy a)) instance VGM.MVector VUM.MVector (Foo a)
-- >>> deriving via (Foo a `VU.As` (Int, VU.DoNotUnboxLazy a)) instance VG.Vector   VU.Vector   (Foo a)
-- >>> instance VU.Unbox (Foo a)
-- >>> :}
--
-- >>> VU.fromListN 3 [ Foo 4 "Haskell's", Foo 8 "strong", Foo 16 "types" ]
-- [Foo 4 "Haskell's",Foo 8 "strong",Foo 16 "types"]
--
-- @since 0.13.2.0
newtype DoNotUnboxLazy a = DoNotUnboxLazy a

newtype instance VUM.MVector s (DoNotUnboxLazy a) = MV_DoNotUnboxLazy (VM.MVector s a)

newtype instance VU.Vector (DoNotUnboxLazy a) = V_DoNotUnboxLazy (V.Vector a)

instance VGM.MVector VUM.MVector (DoNotUnboxLazy a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength = coerce $ VGM.basicLength @V.MVector @a
  basicUnsafeSlice = coerce $ VGM.basicUnsafeSlice @V.MVector @a
  basicOverlaps = coerce $ VGM.basicOverlaps @V.MVector @a
  basicUnsafeNew = coerce $ VGM.basicUnsafeNew @V.MVector @a
  basicInitialize = coerce $ VGM.basicInitialize @V.MVector @a
  basicUnsafeReplicate = coerce $ VGM.basicUnsafeReplicate @V.MVector @a
  basicUnsafeRead = coerce $ VGM.basicUnsafeRead @V.MVector @a
  basicUnsafeWrite = coerce $ VGM.basicUnsafeWrite @V.MVector @a
  basicClear = coerce $ VGM.basicClear @V.MVector @a
  basicSet = coerce $ VGM.basicSet @V.MVector @a
  basicUnsafeCopy = coerce $ VGM.basicUnsafeCopy @V.MVector @a
  basicUnsafeMove = coerce $ VGM.basicUnsafeMove @V.MVector @a
  basicUnsafeGrow = coerce $ VGM.basicUnsafeGrow @V.MVector @a

instance VG.Vector VU.Vector (DoNotUnboxLazy a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze = coerce $ VG.basicUnsafeFreeze @V.Vector @a
  basicUnsafeThaw = coerce $ VG.basicUnsafeThaw @V.Vector @a
  basicLength = coerce $ VG.basicLength @V.Vector @a
  basicUnsafeSlice = coerce $ VG.basicUnsafeSlice @V.Vector @a
  basicUnsafeIndexM = coerce $ VG.basicUnsafeIndexM @V.Vector @a
  basicUnsafeCopy = coerce $ VG.basicUnsafeCopy @V.Vector @a
  elemseq _ = seq

instance VU.Unbox (DoNotUnboxLazy a)
