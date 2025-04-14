{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}

-- | @ModInt@ for 64 bit modulus values with Montgomery modular multiplication.
--
-- ==== Constraints
-- - The modulus value should be an odd number, otherwise it would be too slow.
--
-- @since 1.2.6.0
module AtCoder.Extra.ModInt64
  ( -- * ModInt64
    ModInt64 (..),

    -- * Constructors

    -- ** Safe constructors
    new,
    new64,

    -- ** Unsafe constructor
    unsafeNew,

    -- * Accessors

    -- ** Modulus value
    modulus,

    -- ** Internal value
    val,
    val64,

    -- * Operators
    pow,
    inv,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Extra.Math.Montgomery64 qualified as M64
import Data.Ratio (denominator, numerator)
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Primitive qualified as P
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word64)
import GHC.Exts (proxy#)
import GHC.Stack (HasCallStack)
import GHC.TypeNats (KnownNat, natVal')

-- | `Word64` value that treats the modular arithmetic.
--
-- ==== Constraints
-- - The modulus value should be an odd number, otherwise it would be too slow.
--
-- @since 1.2.6.0
newtype ModInt64 a = ModInt64
  { -- | Montgomery form of the value. Use `val` to retrieve the value.
    --
    -- @since 1.2.6.0
    unModInt64 :: Word64
  }
  deriving
    ( -- | @since 1.2.6.0
      P.Prim
    )

-- | @since 1.2.6.0
instance (KnownNat a) => Eq (ModInt64 a) where
  {-# INLINE (==) #-}
  ModInt64 x == ModInt64 y = M64.eq (fromIntegral (natVal' (proxy# @a))) x y

-- | @since 1.2.6.0
instance (KnownNat a) => Ord (ModInt64 a) where
  {-# INLINE compare #-}
  compare (ModInt64 a) (ModInt64 b) = compare a b

-- | @since 1.2.6.0
instance (KnownNat a) => Read (ModInt64 a) where
  {-# INLINE readsPrec #-}
  readsPrec p s = [(fromInteger x, r) | (!x, !r) <- readsPrec p s]

-- | @since 1.2.6.0
instance (KnownNat a) => Show (ModInt64 a) where
  {-# INLINE show #-}
  show = show . val

-- | \(O(1)\) Creates a `ModInt64` from an `Int` value taking the mod.
--
-- @since 1.2.6.0
{-# INLINE new #-}
new :: forall a. (KnownNat a) => Int -> ModInt64 a
new = ModInt64 . M64.encode (M64.new (proxy# @a)) . fromIntegral . (`mod` m)
  where
    !m = fromIntegral $ natVal' (proxy# @a)

-- | \(O(1)\) Creates a `ModInt64` from a `Word64` value taking the mod.
--
-- @since 1.2.6.0
{-# INLINE new64 #-}
new64 :: forall a. (KnownNat a) => Word64 -> ModInt64 a
new64 = ModInt64 . M64.encode (M64.new (proxy# @a))

-- | \(O(1)\) Creates `ModInt64` from a Montgomery form with no validation.
--
-- @since 1.2.6.0
{-# INLINE unsafeNew #-}
unsafeNew :: (KnownNat a) => Word64 -> ModInt64 a
unsafeNew = ModInt64

-- | \(O(1)\) Retrieve the mod from a `ModInt64` object.
--
-- ==== Complecity
-- - \(O(1)\)
--
-- @since 1.2.6.0
{-# INLINE modulus #-}
modulus :: forall a. (KnownNat a) => ModInt64 a -> Int
modulus _ = fromIntegral (natVal' (proxy# @a))

-- | \(O(1)\) Returns the internal value in `Int`.
--
-- ==== Complecity
-- - \(O(1)\)
--
-- @since 1.2.6.0
{-# INLINE val #-}
val :: forall a. (KnownNat a) => ModInt64 a -> Int
val = fromIntegral . val64

-- | \(O(1)\) Returns the internal value in `Word64`.
--
-- ==== Complecity
-- - \(O(1)\)
--
-- @since 1.2.6.0
{-# INLINE val64 #-}
val64 :: forall a. (KnownNat a) => ModInt64 a -> Word64
val64 (ModInt64 x) = M64.decode (M64.new (proxy# @a)) x

-- | \(O(\log n\) Returns \(x^n\). The implementation is a bit more efficient than `^`.
--
-- ==== Constraints
-- - \(0 \le n\)
--
-- @since 1.2.6.0
{-# INLINE pow #-}
pow :: forall a. (HasCallStack, KnownNat a) => ModInt64 a -> Int -> ModInt64 a
pow (ModInt64 x) n = ModInt64 $! M64.powMod (M64.new (proxy# @a)) x n

-- TODO: move invMod to Montgomery64
-- TODO: time complexity of `inv`?

-- | Returns \(y\) such that \(xy \equiv 1\) holds.
--
-- ==== Constraints
-- - The value must not be zero.
--
-- @since 1.2.6.0
{-# INLINE inv #-}
inv :: forall a. (HasCallStack, KnownNat a) => ModInt64 a -> ModInt64 a
-- TODO: assert zero division?
inv self = inner (val self) m 1 0
  where
    !_ = ACIA.runtimeAssert (val self /= 0) "AtCoder.Extra.ModInt64.inv: given zero"
    !m = fromIntegral (natVal' (proxy# @a))
    inner x y u v
      | y <= 0 = new u
      | otherwise = inner x' y' u' v'
      where
        x' = y
        y' = x - t * y
        u' = v
        v' = u - t * v
        t = x `div` y

-- https://github.com/NyaanNyaan/library/blob/master/modint/montgomery-modint.hpp
-- constexpr mint inverse() const {
--   int x = get(), y = mod, u = 1, v = 0, t = 0, tmp = 0;
--   while (y > 0) {
--     t = x / y;
--     x -= t * y, u -= t * v;
--     tmp = x, x = y, y = tmp;
--     tmp = u, u = v, v = tmp;
--   }
--   return mint{u};
-- }

-- | @since 1.2.6.0
deriving newtype instance (KnownNat p) => Real (ModInt64 p)

-- | @since 1.2.6.0
instance forall p. (KnownNat p) => Num (ModInt64 p) where
  {-# INLINE (+) #-}
  (ModInt64 !x1) + (ModInt64 !x2) = ModInt64 $! M64.addMod m x1 x2
    where
      !m = fromIntegral (natVal' (proxy# @p))
  {-# INLINE (-) #-}
  (ModInt64 !x1) - (ModInt64 !x2) = ModInt64 $! M64.subMod m x1 x2
    where
      !m = fromIntegral (natVal' (proxy# @p))
  {-# INLINE (*) #-}
  (ModInt64 !x1) * (ModInt64 !x2) = ModInt64 $! M64.mulMod (M64.new (proxy# @p)) x1 x2
  {-# INLINE negate #-}
  negate x = 0 - x
  {-# INLINE abs #-}
  abs = id
  {-# INLINE signum #-}
  signum _ = ModInt64 $ M64.encode (M64.new (proxy# @p)) 1
  -- because the input value can be negative, be sure to take the mod:
  {-# INLINE fromInteger #-}
  fromInteger = ModInt64 . M64.encode (M64.new (proxy# @p)) . fromInteger . (`mod` m)
    where
      !m = toInteger $ natVal' (proxy# @p)

-- | @since 1.2.6.0
instance (KnownNat p) => Bounded (ModInt64 p) where
  {-# INLINE minBound #-}
  minBound = ModInt64 0
  {-# INLINE maxBound #-}
  maxBound = ModInt64 . M64.encode (M64.new (proxy# @p)) $! fromIntegral (natVal' (proxy# @p)) - 1

-- | @since 1.2.6.0
instance (KnownNat p) => Enum (ModInt64 p) where
  {-# INLINE toEnum #-}
  toEnum = new
  {-# INLINE fromEnum #-}
  fromEnum = fromIntegral . val

-- | @since 1.2.6.0
instance (KnownNat p) => Integral (ModInt64 p) where
  {-# INLINE quotRem #-}
  quotRem x y = (x / y, x - x / y * y)
  {-# INLINE toInteger #-}
  toInteger = toInteger . val

-- | @since 1.2.6.0
instance (KnownNat p) => Fractional (ModInt64 p) where
  {-# INLINE recip #-}
  recip = inv
  {-# INLINE fromRational #-}
  fromRational q = fromInteger (numerator q) / fromInteger (denominator q)

-- | @since 1.2.6.0
newtype instance VU.MVector s (ModInt64 a) = MV_ModInt64 (VU.MVector s Word64)

-- | @since 1.2.6.0
newtype instance VU.Vector (ModInt64 a) = V_ModInt64 (VU.Vector Word64)

-- | @since 1.2.6.0
deriving newtype instance VGM.MVector VU.MVector (ModInt64 a)

-- | @since 1.2.6.0
deriving newtype instance VG.Vector VU.Vector (ModInt64 a)

-- | @since 1.2.6.0
instance VU.Unbox (ModInt64 a)
