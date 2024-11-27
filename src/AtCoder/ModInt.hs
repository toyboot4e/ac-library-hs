{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}

-- | It is the struct that treats the modular arithmetic. All the remaining parts of AC Library
-- works without modint, so you don't necessarily read this to use the remaining parts.
--
-- For most of the problems, it is sufficient to use `ModInt998244353`, `ModInt1000000007`, or
-- `modInt`, which can be used as follows.
--
-- FIXME: write example
--
-- = Major changes from the original @ac-library@
-- @DynamicModInt@ is removed.
module AtCoder.ModInt
  ( Modulus (..),
    ModInt998244353,
    ModInt1000000007,
    StaticModInt (..),
    modVal,
    modVal#,
    -- TODO: polish API
    new,
    new32,
    modulus,
    raw,
    raw32,
    val,
    val32,
    pow,
    inv,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Math qualified as ACIM
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
import GHC.TypeLits (KnownNat, natVal, natVal')

-- | `KnownNat` with meta information used for modulus.
class (KnownNat a) => Modulus a where
  isPrimeModulus :: Proxy# a -> Bool
  -- | Be sure to override for the speed.
  primitiveRootModulus :: Proxy# a -> Int
  primitiveRootModulus _ = ACIM.primitiveRoot $ fromInteger (natVal' (proxy# @a))

instance Modulus 2 where
  isPrimeModulus _ = True
  primitiveRootModulus _ = 1

instance Modulus 3 where
  isPrimeModulus _ = True
  primitiveRootModulus _ = 2

instance Modulus 167772161 where
  isPrimeModulus _ = True
  primitiveRootModulus _ = 3

instance Modulus 469762049 where
  isPrimeModulus _ = True
  primitiveRootModulus _ = 3

instance Modulus 754974721 where
  isPrimeModulus _ = True
  primitiveRootModulus _ = 11

instance Modulus 998244353 where
  isPrimeModulus _ = True
  primitiveRootModulus _ = 3

instance Modulus 1000000007 where
  isPrimeModulus _ = True
  primitiveRootModulus _ = 5

type ModInt998244353 = StaticModInt 998244353

type ModInt1000000007 = StaticModInt 1000000007

-- type ModInt998244353 = DynamicModInt (-1);

newtype StaticModInt a = StaticModInt {unStaticModInt :: Word32}
  deriving (P.Prim)
  deriving newtype (Eq, Ord, Read, Show)

-- | Retrieves `Int` from `KnownNat`.
--
-- >>> :set -XDataKinds
-- >>> import Data.Proxy (Proxy(..))
-- >>> import GHC.TypeLits (natVal)
-- >>> fromInteger (natVal (Proxy @42))
-- 42
modVal :: forall a. (KnownNat a) => Proxy a -> Int
modVal p = fromInteger $ natVal p

-- | Retrieves `Int` from `KnownNat`.
--
-- >>> :set -XDataKinds
-- >>> :set -XMagicHash
-- >>> import GHC.Exts (proxy#)
-- >>> import GHC.TypeLits (natVal')
-- >>> fromInteger (natVal' (proxy# @42))
-- 42
modVal# :: forall a. (KnownNat a) => Proxy# a -> Int
modVal# p = fromInteger $ natVal' p

new :: forall a. (KnownNat a) => Int -> StaticModInt a
new v = StaticModInt . fromIntegral $! v `mod` fromInteger (natVal' (proxy# @a))

new32 :: forall a. (KnownNat a) => Word32 -> StaticModInt a
new32 v = StaticModInt $! v `mod` fromInteger (natVal' (proxy# @a))

modulus :: forall a. (KnownNat a) => StaticModInt a -> Int
modulus _ = fromInteger (natVal' (proxy# @a))

raw :: (KnownNat a) => Int -> StaticModInt a
raw = StaticModInt . fromIntegral

raw32 :: (KnownNat a) => Word32 -> StaticModInt a
raw32 = StaticModInt

val :: (KnownNat a) => StaticModInt a -> Int
val = fromIntegral . unStaticModInt

val32 :: (KnownNat a) => StaticModInt a -> Word32
val32 = unStaticModInt

pow :: (HasCallStack, KnownNat a) => StaticModInt a -> Int -> StaticModInt a
pow x0 n0 = inner x0 n0 1
  where
    !_ = ACIA.runtimeAssert (0 <= n0) $ "AtCoder.ModInt.pow: given negative exponential `n`: " ++ show n0 ++ show "`"
    inner !x !n !r
      | n == 0 = r
      | otherwise =
          let !r' = if odd n then r * x else r
           in inner (x * x) (n .>>. 1) r'

inv :: forall a. (HasCallStack, Modulus a) => StaticModInt a -> StaticModInt a
inv self@(StaticModInt x)
  | isPrimeModulus (proxy# @a) =
      let !_ = ACIA.runtimeAssert (x /= 0) "AtCoder.ModInt.inv: tried to perform zero division"
       in pow self (fromInteger (natVal' (proxy# @a)) - 2)
  | otherwise =
      let (!eg1, !eg2) = ACIM.invGcd (fromIntegral x) $ fromInteger (natVal' (proxy# @a))
          !_ = ACIA.runtimeAssert (eg1 == 1) "AtCoder.ModInt.inv: `gcd x mod` not equals to `1`"
       in fromIntegral eg2

deriving newtype instance (KnownNat p) => Real (StaticModInt p)

instance (KnownNat p) => Num (StaticModInt p) where
  (StaticModInt !x1) + (StaticModInt !x2)
    | x' >= m = StaticModInt $! x' - m
    | otherwise = StaticModInt x'
    where
      !x' = x1 + x2
      !m = fromInteger (natVal' (proxy# @p))
  (StaticModInt !x1) - (StaticModInt !x2)
    | x' >= m = StaticModInt $! x' + m -- loops
    | otherwise = StaticModInt x'
    where
      !x' = x1 - x2
      !m = fromInteger (natVal' (proxy# @p))
  (StaticModInt !x1) * (StaticModInt !x2) = StaticModInt $! fromIntegral (x' `rem` m)
    where
      !x' :: Word64 = fromIntegral x1 * fromIntegral x2
      !m :: Word64 = fromInteger (natVal' (proxy# @p))
  negate x = 0 - x
  abs = id
  signum _ = StaticModInt 1
  fromInteger = StaticModInt . fromInteger . (`mod` natVal' (proxy# @p))

instance (KnownNat p) => Bounded (StaticModInt p) where
  minBound = StaticModInt 0
  maxBound = StaticModInt $! fromInteger (natVal' (proxy# @p)) - 1

instance (KnownNat p) => Enum (StaticModInt p) where
  toEnum = new
  fromEnum = fromIntegral . unStaticModInt

instance (Modulus p) => Integral (StaticModInt p) where
  quotRem x y = (x / y, x - x / y * y)
  toInteger = coerce (toInteger @Word32)

instance (Modulus p) => Fractional (StaticModInt p) where
  recip = inv
  fromRational q = fromInteger (numerator q) / fromInteger (denominator q)

newtype instance VU.MVector s (StaticModInt a) = MV_StaticModInt (VU.MVector s Word32)

newtype instance VU.Vector (StaticModInt a) = V_StaticModInt (VU.Vector Word32)

deriving newtype instance VGM.MVector VU.MVector (StaticModInt a)

deriving newtype instance VG.Vector VU.Vector (StaticModInt a)

instance VU.Unbox (StaticModInt a)
