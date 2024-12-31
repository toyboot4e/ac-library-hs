{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}

-- | It is the structure that treats the modular arithmetic. All the remaining parts of AC Library
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
-- ==== Major changes from the original @ac-library@
-- - @DynamicModInt@ is removed.
--
-- @since 1.0.0.0
module AtCoder.ModInt
  ( -- * Modulus
    Modulus (..),
    ModInt998244353,
    ModInt1000000007,

    -- ** Helpers
    modVal,
    modVal#,

    -- * ModInt
    ModInt (..),

    -- * Constructors

    -- ** Safe constructors
    new,
    new32,
    new64,

    -- ** Unsafe constructor
    unsafeNew,

    -- * Accessors

    -- ** Modulus value
    modulus,

    -- ** Internal value
    val,
    val32,
    val64,

    -- * Operators
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
--
-- @since 1.0.0.0
class (KnownNat a) => Modulus a where
  -- | Returns if the modulus is a prime value.
  --
  -- @since 1.0.0.0
  isPrimeModulus :: Proxy# a -> Bool

  -- | Returns the primitive root of the modulus value. Note that the default implementation is
  -- slow.
  --
  -- @since 1.0.0.0
  {-# INLINE primitiveRootModulus #-}
  primitiveRootModulus :: Proxy# a -> Int
  -- we could use `AllowAmbigousTypes` or `Tagged` newtype, but `Proxy#` wasn't so slow.
  -- not sure about `x^n` case though..
  primitiveRootModulus _ = ACIM.primitiveRoot $ fromIntegral (natVal' (proxy# @a))

-- | \(2^{24} - 1\).
--
-- @since 1.0.0.0
instance Modulus 167772161 where
  {-# INLINE isPrimeModulus #-}
  isPrimeModulus _ = True
  {-# INLINE primitiveRootModulus #-}
  primitiveRootModulus _ = 3

-- | \(2^{25} - 1\).
--
-- @since 1.0.0.0
instance Modulus 469762049 where
  {-# INLINE isPrimeModulus #-}
  isPrimeModulus _ = True
  {-# INLINE primitiveRootModulus #-}
  primitiveRootModulus _ = 3

-- | \(2^{26} - 1\).
--
-- @since 1.0.0.0
instance Modulus 754974721 where
  {-# INLINE isPrimeModulus #-}
  isPrimeModulus _ = True
  {-# INLINE primitiveRootModulus #-}
  primitiveRootModulus _ = 11

-- | \(119 \times 2^{23} + 1\). It is often used in contest problems
--
-- @since 1.0.0.0
instance Modulus 998244353 where
  {-# INLINE isPrimeModulus #-}
  isPrimeModulus _ = True
  {-# INLINE primitiveRootModulus #-}
  primitiveRootModulus _ = 3

-- | It used to be used in contest problems.
--
-- @since 1.0.0.0
instance Modulus 1000000007 where
  {-# INLINE isPrimeModulus #-}
  isPrimeModulus _ = True
  {-# INLINE primitiveRootModulus #-}
  primitiveRootModulus _ = 5

-- | \(2^{31} - 1\), suitable for boundary testing.
--
-- @since 1.0.0.0
instance Modulus 2147483647 where
  {-# INLINE isPrimeModulus #-}
  isPrimeModulus _ = True
  {-# INLINE primitiveRootModulus #-}
  primitiveRootModulus _ = 7

-- | `ModInt` with modulus value @998244353@.
--
-- @since 1.0.0.0
type ModInt998244353 = ModInt 998244353

-- | `ModInt` with modulus value @1000000007@.
--
-- @since 1.0.0.0
type ModInt1000000007 = ModInt 1000000007

-- | Retrieves `Int` from `KnownNat`.
--
-- >>> import Data.Proxy (Proxy(..))
-- >>> modVal (Proxy @42)
-- 42
--
-- @since 1.0.0.0
{-# INLINE modVal #-}
modVal :: forall a. (KnownNat a) => Proxy a -> Int
modVal p = fromIntegral $ natVal p

-- | Retrieves `Int` from `KnownNat`.
--
-- >>> :set -XMagicHash
-- >>> import GHC.Exts (proxy#)
-- >>> modVal# (proxy# @42)
-- 42
--
-- @since 1.0.0.0
{-# INLINE modVal# #-}
modVal# :: forall a. (KnownNat a) => Proxy# a -> Int
modVal# p = fromIntegral $ natVal' p

-- | Creates `ModInt` from an `Int` value taking mod.
--
-- @since 1.0.0.0
{-# INLINE new #-}
new :: forall a. (KnownNat a) => Int -> ModInt a
new v = ModInt . fromIntegral $ v `mod` fromIntegral (natVal' (proxy# @a))

-- | Creates `ModInt` from a `Word32` value taking mod.
--
-- @since 1.0.0.0
{-# INLINE new32 #-}
new32 :: forall a. (KnownNat a) => Word32 -> ModInt a
new32 v = ModInt $ v `mod` fromIntegral (natVal' (proxy# @a))

-- | Creates `ModInt` from a `Word64` value taking mod.
--
-- @since 1.0.0.0
{-# INLINE new64 #-}
new64 :: forall a. (KnownNat a) => Word64 -> ModInt a
new64 v = ModInt . fromIntegral $ v `mod` fromIntegral (natVal' (proxy# @a))

-- | Creates `ModInt` without taking mod. It is the function for constant-factor speedup.
--
-- ==== Constraints
-- - \(0 \leq x \lt \mathrm{mod}\) (not asserted at runtime)
--
-- @since 1.0.0.0
{-# INLINE unsafeNew #-}
unsafeNew :: (KnownNat a) => Word32 -> ModInt a
unsafeNew = ModInt

-- | `Word32` value that treats the modula arithmetic.
newtype ModInt a = ModInt {unModInt :: Word32}
  deriving
    ( -- @since 1.0.0.0
      P.Prim
    )
  deriving newtype
    ( -- @since 1.0.0.0
      Eq,
      -- @since 1.0.0.0
      Ord,
      -- @since 1.0.0.0
      Read,
      -- @since 1.0.0.0
      Show
    )

-- | Retrieve the mod from a `ModInt` object.
--
-- ==== Complecity
-- - \(O(1)\)
--
-- @since 1.0.0.0
{-# INLINE modulus #-}
modulus :: forall a. (KnownNat a) => ModInt a -> Int
modulus _ = fromIntegral (natVal' (proxy# @a))

-- | Returns the internal value converted to `Int`.
--
-- ==== Complecity
-- - \(O(1)\)
--
-- @since 1.0.0.0
{-# INLINE val #-}
val :: (KnownNat a) => ModInt a -> Int
val = fromIntegral . unModInt

-- | Returns the internal value as `Word32` without type conversion. It is the function for
-- constant-factor speedup.
--
-- ==== Complecity
-- - \(O(1)\)
--
-- @since 1.0.0.0
{-# INLINE val32 #-}
val32 :: (KnownNat a) => ModInt a -> Word32
val32 = unModInt

-- | Returns the internal value converted to `Word32`.
--
-- ==== Complecity
-- - \(O(1)\)
--
-- @since 1.0.0.0
{-# INLINE val64 #-}
val64 :: (KnownNat a) => ModInt a -> Word64
val64 = fromIntegral . unModInt

-- | Returns \(x^n\). The implementation is a bit more efficient than `^`.
--
-- ==== Constraints
-- - \(0 \le n\)
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- @since 1.0.0.0
{-# INLINE pow #-}
pow :: forall a. (HasCallStack, KnownNat a) => ModInt a -> Int -> ModInt a
pow (ModInt x0) n0 = ModInt . fromIntegral $ inner n0 1 (fromIntegral x0)
  where
    !_ = ACIA.runtimeAssert (0 <= n0) $ "AtCoder.ModInt.pow: given negative exponential `n`: " ++ show n0 ++ show "`"
    bt = ACIBT.new64 $ fromIntegral (natVal' (proxy# @a))
    inner :: Int -> Word64 -> Word64 -> Word64
    inner !n !r !y
      | n == 0 = r
      | otherwise =
          let r' = if odd n then ACIBT.mulMod bt r y else r
              y' = ACIBT.mulMod bt y y
           in inner (n !>>. 1) r' y'

-- Original ACL version seems like slower as in the benchmark
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
-- ==== Constraints
-- - @\gcd(val x, modulus x) == 1@.
--
-- ==== Complexity
-- - \(O(\log \mathrm{mod})\)
--
-- @since 1.0.0.0
{-# INLINE inv #-}
inv :: forall a. (HasCallStack, Modulus a) => ModInt a -> ModInt a
inv self@(ModInt x)
  | isPrimeModulus (proxy# @a) =
      let !_ = ACIA.runtimeAssert (x /= 0) "AtCoder.ModInt.inv: tried to perform zero division"
       in pow self (fromIntegral (natVal' (proxy# @a)) - 2)
  | otherwise =
      let (!eg1, !eg2) = ACIM.invGcd (fromIntegral x) $ fromIntegral (natVal' (proxy# @a))
          !_ = ACIA.runtimeAssert (eg1 == 1) "AtCoder.ModInt.inv: `x^(-1) mod m` cannot be calculated when `gcd x modulus /= 1`"
       in fromIntegral eg2

-- | -- @since 1.0.0.0
deriving newtype instance (KnownNat p) => Real (ModInt p)

-- | -- @since 1.0.0.0
instance (KnownNat p) => Num (ModInt p) where
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
  (ModInt !x1) * (ModInt !x2) = ModInt $! fromIntegral (x' `rem` m)
    where
      !x' :: Word64 = fromIntegral x1 * fromIntegral x2
      !m :: Word64 = fromIntegral (natVal' (proxy# @p))
  {-# INLINE negate #-}
  negate x = 0 - x
  {-# INLINE abs #-}
  abs = id
  {-# INLINE signum #-}
  signum _ = ModInt 1
  {-# INLINE fromInteger #-}
  fromInteger = ModInt . fromInteger . (`mod` fromIntegral (natVal' (proxy# @p)))

-- | -- @since 1.0.0.0
instance (KnownNat p) => Bounded (ModInt p) where
  {-# INLINE minBound #-}
  minBound = ModInt 0
  {-# INLINE maxBound #-}
  maxBound = ModInt $! fromIntegral (natVal' (proxy# @p)) - 1

-- | -- @since 1.0.0.0
instance (KnownNat p) => Enum (ModInt p) where
  {-# INLINE toEnum #-}
  toEnum = new
  {-# INLINE fromEnum #-}
  fromEnum = fromIntegral . unModInt

-- | -- @since 1.0.0.0
instance (Modulus p) => Integral (ModInt p) where
  {-# INLINE quotRem #-}
  quotRem x y = (x / y, x - x / y * y)
  {-# INLINE toInteger #-}
  toInteger = coerce (toInteger @Word32)

-- | -- @since 1.0.0.0
instance (Modulus p) => Fractional (ModInt p) where
  {-# INLINE recip #-}
  recip = inv
  {-# INLINE fromRational #-}
  fromRational q = fromInteger (numerator q) / fromInteger (denominator q)

-- | -- @since 1.0.0.0
newtype instance VU.MVector s (ModInt a) = MV_ModInt (VU.MVector s Word32)

-- | -- @since 1.0.0.0
newtype instance VU.Vector (ModInt a) = V_ModInt (VU.Vector Word32)

-- | -- @since 1.0.0.0
deriving newtype instance VGM.MVector VU.MVector (ModInt a)

-- | -- @since 1.0.0.0
deriving newtype instance VG.Vector VU.Vector (ModInt a)

-- | -- @since 1.0.0.0
instance VU.Unbox (ModInt a)
