{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}

-- | TypeNats-based.
module BenchLib.ModInt.ModIntNats
  ( ModInt (..),
    unsafePow,
    inv,
    unsafeNew,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Math qualified as ACIM
import BenchLib.ModInt.Modulus
import BenchLib.MulMod.BarrettWideWord qualified as BarrettWideWord
import Data.Bits
import Data.Coerce (coerce)
import Data.Proxy (Proxy)
import Data.Ratio (denominator, numerator)
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Primitive qualified as P
import Data.Vector.Unboxed qualified as U
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word32, Word64)
import GHC.Exts (Proxy#, proxy#)
import GHC.Stack (HasCallStack)
import GHC.TypeNats (KnownNat, natVal, natVal')

-- TODO: which modulus is fatest?

newtype ModInt a = ModInt {unModInt :: Word32}
  deriving (P.Prim)
  deriving newtype (Eq, Ord, Read, Show)

{-# INLINE unsafeNew #-}
unsafeNew :: (KnownNat a) => Word32 -> ModInt a
unsafeNew = coerce

-- {-# INLINE unsafePow #-}
unsafePow :: forall a. (Modulus a) => ModInt a -> Int -> ModInt a
unsafePow (ModInt x0) n0 = unsafeNew . fromIntegral $ inner n0 1 (fromIntegral x0)
  where
    bt = BarrettWideWord.new64 $ fromIntegral (natVal' (proxy# @a))
    inner :: Int -> Word64 -> Word64 -> Word64
    inner !n !r !y
      | n == 0 = r
      | otherwise =
          let r' = if odd n then BarrettWideWord.mulMod bt r y else r
              y' = BarrettWideWord.mulMod bt y y
           in inner (n !>>. 1) r' y'

-- ACL version was slower.
-- unsafePow :: forall a. (Modulus a) => ModInt a -> Int -> ModInt a
-- unsafePow x0 n0 = inner x0 n0 (unsafeNew 1)
--   where
--     inner :: ModInt a -> Int -> ModInt a -> ModInt a
--     inner !x !n !r
--       | n == 0 = r
--       | otherwise =
--           let !r' = if odd n then r * x else r
--            in inner (x * x) (n !>>. 1) r'

inv :: forall a. (HasCallStack, Modulus a) => ModInt a -> ModInt a
inv self@(ModInt x)
  | isPrimeModulus (proxy# @a) =
      let !_ = ACIA.runtimeAssert (x /= 0) "AtCoder.ModInt.inv: tried to perform zero division"
       in -- FIXME: use pow
          unsafePow self (fromIntegral (natVal' (proxy# @a)) - 2)
  | otherwise =
      let (!eg1, !eg2) = ACIM.invGcd (fromIntegral x) $ fromIntegral (natVal' (proxy# @a))
          !_ = ACIA.runtimeAssert (eg1 == 1) "AtCoder.ModInt.inv: `x^(-1) mod m` cannot be calculated when `gcd x modulus /= 1`"
       in fromIntegral eg2

deriving newtype instance (Modulus p) => Real (ModInt p)

instance (Modulus p) => Num (ModInt p) where
  {-# INLINE (+) #-}
  (ModInt !x1) + (ModInt !x2)
    | x' >= m = ModInt $! x' - m
    | otherwise = ModInt x'
    where
      !x' = x1 + x2
      !m = fromIntegral (natVal' (proxy# @p))
  {-# INLINE (-) #-}
  (ModInt !x1) - (ModInt !x2)
    | x' >= m = ModInt $! x' + m -- loops
    | otherwise = ModInt x'
    where
      !x' = x1 - x2
      !m = fromIntegral (natVal' (proxy# @p))
  {-# INLINE (*) #-}
  (ModInt !x1) * (ModInt !x2) = coerce $! (fromIntegral (x' `rem` m) :: Word32)
    where
      !x' :: Word64 = fromIntegral x1 * fromIntegral x2
      !m :: Word64 = coerce $ amb64Modulus @p
  negate x = 0 - x
  abs = id
  signum _ = ModInt 1
  fromInteger = ModInt . fromInteger . (`mod` fromIntegral (natVal' (proxy# @p)))

instance (Modulus p) => Bounded (ModInt p) where
  minBound = ModInt 0
  maxBound = ModInt $! fromIntegral (natVal' (proxy# @p)) - 1

instance (Modulus p) => Enum (ModInt p) where
  toEnum = unsafeNew . fromIntegral . (`mod` fromIntegral (natVal' (proxy# @p)))
  fromEnum = fromIntegral . unModInt

instance (Modulus p) => Integral (ModInt p) where
  quotRem x y = (x / y, x - x / y * y)
  toInteger = coerce (toInteger @Word32)

instance (Modulus p) => Fractional (ModInt p) where
  recip = inv
  fromRational q = fromInteger (numerator q) / fromInteger (denominator q)

newtype instance VU.MVector s (ModInt a) = MV_ModInt (VU.MVector s Word32)

newtype instance VU.Vector (ModInt a) = V_ModInt (VU.Vector Word32)

deriving newtype instance VGM.MVector VU.MVector (ModInt a)

deriving newtype instance VG.Vector VU.Vector (ModInt a)

instance VU.Unbox (ModInt a)
