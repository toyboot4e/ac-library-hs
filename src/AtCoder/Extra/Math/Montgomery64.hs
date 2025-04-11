{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Fast modular multiplication for `Word64` using Montgomery multiplication. If the modulus value
-- is known to fit in 32 bits, use the @AtCoder.Internal.Barrett@ module instead.
--
-- @since 1.2.6.0
module AtCoder.Extra.Math.Montgomery64
  ( -- * Montgomery64
    Montgomery64,

    -- * Constructor
    new,
    fromVal,

    -- * Accessor
    umod,

    -- * Montgomery form encoding
    encode,
    decode,
    reduce,

    -- * Calculations
    addMod,
    subMod,
    mulMod,
    powMod,
    eq,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import Data.Bits (bit, (!>>.))
import Data.WideWord.Word128 (Word128 (..))
import Data.Word (Word64)
import GHC.Exts (Proxy#)
import GHC.Stack (HasCallStack)
import GHC.TypeNats (KnownNat, natVal')

-- TODO: provide with newtype for Montgomery form?

-- | Fast modular multiplication for `Word64` using Montgomery64 multiplication.
--
-- @since 1.2.6.0
data Montgomery64 = Montgomery64
  { mM64 :: {-# UNPACK #-} !Word64,
    rM64 :: {-# UNPACK #-} !Word64,
    n2M64 :: {-# UNPACK #-} !Word64
  }
  deriving
    ( -- | @since 1.2.6.0
      Eq,
      -- | @since 1.2.6.0
      Show
    )

-- TODO: add unasfePerformIO?
-- TODO: remove NOINLINE?

-- | \(O(1)\) Static, shared storage of `Montgomery64`.
--
-- ==== Constraints
-- - \(m \le 2^{62})
-- - \(m\) is odd
--
-- @since 1.2.6.0
{-# NOINLINE new #-}
new :: forall a. (KnownNat a) => Proxy# a -> Montgomery64
-- FIXME: test allocated once
new p = fromVal . fromIntegral $! natVal' p

-- | \(O(1)\) Creates a `Montgomery64` for a modulus value \(m\) of type `Word64` value.
--
-- ==== Constraints
-- - \(m \le 2^{62})
-- - \(m\) is odd
--
-- @since 1.2.6.0
{-# INLINE fromVal #-}
fromVal :: Word64 -> Montgomery64
fromVal m =
  let !m128 :: Word128 = fromIntegral m
      !n2 = word128Lo64 $ (-m128) `mod` m128
      !r = getR m 0
      !_ = ACIA.runtimeAssert (r * m == -1) "AtCoder.Extra.Montgomery64.fromVal: internal implementation error"
   in Montgomery64 m r n2
  where
    !_ = ACIA.runtimeAssert (odd m && m <= bit 62) $ "AtCoder.Extra.Montgomery64.fromVal: not given odd modulus value that is less than or equal to 2^62: " ++ show m
    getR :: Word64 -> Int -> Word64
    getR !acc i
      | i >= 5 = -acc
      | otherwise = getR (acc * (2 - m * acc)) (i + 1)

-- | \(O(1)\) Retrieves the modulus \(m\).
--
-- @since 1.2.6.0
{-# INLINE umod #-}
umod :: Montgomery64 -> Word64
umod Montgomery64 {mM64} = mM64

-- | \(O(1)\) Converts the given `Word64` to Montgomery form.
--
-- @since 1.2.6.0
{-# INLINE encode #-}
encode :: Montgomery64 -> Word64 -> Word64
encode mont@Montgomery64 {n2M64} x = reduce mont $! fromIntegral x * fromIntegral n2M64

-- | \(O(1)\) Retrieves the value from a Montgomery form of value.
--
-- @since 1.2.6.0
{-# INLINE decode #-}
decode :: Montgomery64 -> Word64 -> Word64
decode mont@Montgomery64 {mM64} x =
  let !res = reduce mont $! fromIntegral x
   in if res >= mM64 then res - mM64 else res

-- | \(O(1)\) Takes the mod in Montgomery form.
--
-- @since 1.2.6.0
{-# INLINE reduce #-}
reduce :: Montgomery64 -> Word128 -> Word64
reduce Montgomery64 {mM64, rM64} x =
  word128Hi64 $!
    (x + fromIntegral (word128Lo64 x * rM64) * fromIntegral mM64)

-- | \(O(1)\) Calculates \(a + b \bmod m\) in the Montgomery form.
{-# INLINE addMod #-}
addMod :: Word64 -> Word64 -> Word64 -> Word64
addMod m a b
    | x' >= m = x' - m
    | otherwise = x'
  where
    !x' = a + b

-- | \(O(1)\) Calculates \(a - b \bmod m\) in the Montgomery form.
{-# INLINE subMod #-}
subMod :: Word64 -> Word64 -> Word64 -> Word64
subMod m a b
    | a >= b = a - b
    | otherwise = a - b + m

-- | \(O(1)\) Calculates \(a^n \bmod m\) in the Montgomery form.
--
-- @since 1.2.6.0
{-# INLINE mulMod #-}
mulMod :: Montgomery64 -> Word64 -> Word64 -> Word64
mulMod mont a b = reduce mont $! fromIntegral a * fromIntegral b

-- | \(O(w)\) Calculates \(a^n \bmod m\) in the Montgomery form.
--
-- @since 1.2.6.0
{-# INLINE powMod #-}
powMod :: (HasCallStack) => Montgomery64 -> Word64 -> Int -> Word64
powMod mont x0 n0 = inner n0 (encode mont 1) x0
  where
    !_ = ACIA.runtimeAssert (0 <= n0) $ "AtCoder.Extra.Math.Montgomery64.powMod: given negative exponential `n`: " ++ show n0 ++ show "`"
    inner :: Int -> Word64 -> Word64 -> Word64
    inner !n !r !y
      | n == 0 = r
      | otherwise =
          let !r' = if odd n then mulMod mont r y else r
              !y' = mulMod mont y y
           in inner (n !>>. 1) r' y'

-- | \(O(1)\) Compares two values of Montgomery form and returns whether they represent the same
-- value.
--
-- @since 1.2.6.0
{-# INLINE eq #-}
eq :: Word64 -> Word64 -> Word64 -> Bool
eq mM64 a b = a' == b'
  where
    !a' = if a < mM64 then a else a - mM64
    !b' = if b < mM64 then b else b - mM64

