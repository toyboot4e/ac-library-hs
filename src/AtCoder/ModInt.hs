{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}

-- | It is the struct that treats the modular arithmetic. All the remaining parts of AC Library
-- works without modint, so you don't necessarily read this to use the remaining parts.
--
-- For most of the problems, it is sufficient to use `ModInt998244353`, `ModInt1000000007`, which
-- can be used as follows.
--
-- >>> import AtCoder.ModInt qualified as M
-- >>> type Mint = M.ModInt998244353
-- >>> let modInt :: Int -> Mint; modInt = M.new
-- >>> modInt 1000000000
-- 1755647
--
-- = Major changes from the original @ac-library@
-- - @DynamicModInt@ is removed.
module AtCoder.ModInt
  ( Modulus (..),
    ModInt998244353,
    ModInt1000000007,
    ModInt (..),
    modVal,
    modVal#,
    new,
    new32,
    new64,
    modulus,
    unsafeNew,
    val,
    val32,
    val64,
    pow,
    inv,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Barrett qualified as ACIBT
import AtCoder.Internal.Math qualified as ACIM
import Data.Bits ((!>>.))
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

-- | `KnownNat` with meta information used for modulus.
class (KnownNat a) => Modulus a where
  -- | Returns if
  isPrimeModulus :: Proxy# a -> Bool

  -- | Note that the default implementation is slow.
  {-# INLINE primitiveRootModulus #-}
  primitiveRootModulus :: Proxy# a -> Int
  -- we could use `AllowAmbigousTypes` or `Tagged` newtype, but `Proxy#` wan't slow.
  primitiveRootModulus _ = ACIM.primitiveRoot $ fromIntegral (natVal' (proxy# @a))

instance Modulus 2 where
  {-# INLINE isPrimeModulus #-}
  isPrimeModulus _ = True
  {-# INLINE primitiveRootModulus #-}
  primitiveRootModulus _ = 1

instance Modulus 3 where
  {-# INLINE isPrimeModulus #-}
  isPrimeModulus _ = True
  {-# INLINE primitiveRootModulus #-}
  primitiveRootModulus _ = 2

-- | 2^24 - 1
instance Modulus 167772161 where
  isPrimeModulus _ = True
  {-# INLINE primitiveRootModulus #-}
  primitiveRootModulus _ = 3

-- | 2^25 - 1
instance Modulus 469762049 where
  {-# INLINE isPrimeModulus #-}
  isPrimeModulus _ = True
  {-# INLINE primitiveRootModulus #-}
  primitiveRootModulus _ = 3

-- | 2^26 - 1
instance Modulus 754974721 where
  {-# INLINE isPrimeModulus #-}
  isPrimeModulus _ = True
  {-# INLINE primitiveRootModulus #-}
  primitiveRootModulus _ = 11

instance Modulus 998244353 where
  {-# INLINE isPrimeModulus #-}
  isPrimeModulus _ = True
  {-# INLINE primitiveRootModulus #-}
  primitiveRootModulus _ = 3

instance Modulus 1000000007 where
  {-# INLINE isPrimeModulus #-}
  isPrimeModulus _ = True
  {-# INLINE primitiveRootModulus #-}
  primitiveRootModulus _ = 5

type ModInt998244353 = ModInt 998244353

type ModInt1000000007 = ModInt 1000000007

-- type ModInt998244353 = DynamicModInt (-1);

-- | Retrieves `Int` from `KnownNat`.
--
-- >>> import Data.Proxy (Proxy(..))
-- >>> modVal (Proxy @42)
-- 42
modVal :: forall a. (KnownNat a) => Proxy a -> Int
modVal p = fromIntegral $ natVal p

-- | Retrieves `Int` from `KnownNat`.
--
-- >>> :set -XMagicHash
-- >>> import GHC.Exts (proxy#)
-- >>> modVal# (proxy# 42)
-- 42
modVal# :: forall a. (KnownNat a) => Proxy# a -> Int
modVal# p = fromIntegral $ natVal' p

newtype ModInt a = ModInt {unModInt :: Word32}
  deriving (P.Prim)
  deriving newtype (Eq, Ord, Read, Show)

-- | Creates `ModInt` taking the modulo of an `Int` value.
new :: forall a. (KnownNat a) => Int -> ModInt a
new v = ModInt . fromIntegral $ v `mod` fromIntegral (natVal' (proxy# @a))

-- | Creates `ModInt` taking the modulo of a `Word32` value.
new32 :: forall a. (KnownNat a) => Word32 -> ModInt a
new32 v = ModInt $ v `mod` fromIntegral (natVal' (proxy# @a))

-- | Creates `ModInt` taking the modulo of a `Word64` value.
new64 :: forall a. (KnownNat a) => Word32 -> ModInt a
new64 v = ModInt $ v `mod` fromIntegral (natVal' (proxy# @a))

-- | \(O(1)\) Returns the mod.
modulus :: forall a. (KnownNat a) => ModInt a -> Int
modulus _ = fromIntegral (natVal' (proxy# @a))

-- | \(O(1)\) Returns the internal value converted to `Int`.
val :: (KnownNat a) => ModInt a -> Int
val = fromIntegral . unModInt

-- | \(O(1)\) Returns the internal value as `Word32` without conversion. It is the function for
-- constant-factor speedup.
val32 :: (KnownNat a) => ModInt a -> Word32
val32 = unModInt

-- | \(O(1)\) Returns the internal value converted to `Word32`.
val64 :: (KnownNat a) => ModInt a -> Word32
val64 = fromIntegral . unModInt

-- | Returns \(x^n\). The implementation is a bit more efficient than `^`.
--
-- = Constraints
-- - \(0 \le n\)
--
-- = Complexity
-- - \(O(\log n)\)
pow :: forall a. (HasCallStack, KnownNat a) => ModInt a -> Int -> ModInt a
pow (ModInt x0) n0 = ModInt . fromIntegral $ inner n0 1 (fromIntegral x0)
  where
    !_ = ACIA.runtimeAssert (0 <= n0) $ "AtCoder.ModInt.pow: given negative exponential `n`: " ++ show n0 ++ show "`"
    -- FIXME: cannot calculate correctly
    bt = ACIBT.new64 $ fromIntegral (natVal' (proxy# @a))
    inner :: Int -> Word64 -> Word64 -> Word64
    inner !n !r !y
      | n == 0 = r
      | otherwise =
          let r' = if odd n then ACIBT.mulMod bt r y else r
              y' = ACIBT.mulMod bt y y
           in inner (n !>>. 1) r' y'

-- ACL version
-- pow :: (HasCallStack, KnownNat a) => ModInt a -> Int -> ModInt a
-- pow x0 n0 = inner x0 n0 1
--   where
--     !_ = ACIA.runtimeAssert (0 <= n0) $ "AtCoder.ModInt.pow: given negative exponential `n`: " ++ show n0 ++ show "`"
--     inner !x !n !r
--       | n == 0 = r
--       | otherwise =
--           let !r' = if odd n then r * x else r
--            in inner (x * x) (n !>>. 1) r'

-- | Returns \(y\) with \(xy \equiv 1\).
--
-- = Constraints
-- - @\gcd(val x, modulus x) == 1@.
--
-- = Complexity
-- - \(O(\log \mathrm{mod})\)
inv :: forall a. (HasCallStack, Modulus a) => ModInt a -> ModInt a
inv self@(ModInt x)
  | isPrimeModulus (proxy# @a) =
      let !_ = ACIA.runtimeAssert (x /= 0) "AtCoder.ModInt.inv: tried to perform zero division"
       in pow self (fromIntegral (natVal' (proxy# @a)) - 2)
  | otherwise =
      let (!eg1, !eg2) = ACIM.invGcd (fromIntegral x) $ fromIntegral (natVal' (proxy# @a))
          !_ = ACIA.runtimeAssert (eg1 == 1) "AtCoder.ModInt.inv: `x^(-1) mod m` cannot be calculated when `gcd x modulus /= 1`"
       in fromIntegral eg2

-- | Creates `ModInt` without taking mod. It is the function for constant-factor speedup.
--
-- = Constraints
-- - \(0 \leq x \lt \mathrm{mod}\) (not asserted at runtime)
unsafeNew :: (KnownNat a) => Word32 -> ModInt a
unsafeNew = ModInt

deriving newtype instance (KnownNat p) => Real (ModInt p)

instance (KnownNat p) => Num (ModInt p) where
  (ModInt !x1) + (ModInt !x2)
    | x' >= m = ModInt $! x' - m
    | otherwise = ModInt x'
    where
      !x' = x1 + x2
      !m = fromIntegral (natVal' (proxy# @p))
  (ModInt !x1) - (ModInt !x2)
    | x' >= m = ModInt $! x' + m -- loops
    | otherwise = ModInt x'
    where
      !x' = x1 - x2
      !m = fromIntegral (natVal' (proxy# @p))
  (ModInt !x1) * (ModInt !x2) = ModInt $! fromIntegral (x' `rem` m)
    where
      !x' :: Word64 = fromIntegral x1 * fromIntegral x2
      !m :: Word64 = fromIntegral (natVal' (proxy# @p))
  negate x = 0 - x
  abs = id
  signum _ = ModInt 1
  fromInteger = ModInt . fromInteger . (`mod` fromIntegral (natVal' (proxy# @p)))

instance (KnownNat p) => Bounded (ModInt p) where
  minBound = ModInt 0
  maxBound = ModInt $! fromIntegral (natVal' (proxy# @p)) - 1

instance (KnownNat p) => Enum (ModInt p) where
  toEnum = new
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
