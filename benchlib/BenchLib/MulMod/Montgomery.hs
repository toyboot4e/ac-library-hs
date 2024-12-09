{-# LANGUAGE NamedFieldPuns #-}

-- | Fast modular multiplication by barrett reduction.
--
-- = Example
-- >>> let bt = new 10 -- mod 10
-- >>> umod bt
-- 10
-- >>> mul bt 7 7
-- 9
module BenchLib.MulMod.Montgomery
  ( Montgomery (mMontgomery),
    new,
    umod,
    mulMod,
    mulModGenerated,
    generate,
    reduce,
  )
where

-- FIXME: test
-- TODO: Use MagicHash?

import Data.Bits (bit, (!>>.))
import Data.Word (Word32, Word64)

-- | Fast modular multiplication by Montgomery multiplication. The modulus value has to be odd
-- to be fast.
data Montgomery = Montgomery
  { mMontgomery :: {-# UNPACK #-} !Word64,
    -- | R2 == (2^64) % MOD;
    r2Montgomery :: {-# UNPACK #-} !Word64,
    -- | MOD * NEG_INV % (2^32) == (2^32) - 1;
    negInvMontgomery :: {-# UNPACK #-} !Word32
  }

-- | Creates `Montgomery` for modulus @m@.
new :: Word64 -> Montgomery
new m =
  let !negInv = inner 0 0 1 0
      !r2 = bit 32 `rem` m
      !r2' = r2 * r2 `rem` m
   in Montgomery m r2' $ fromIntegral negInv
  where
    inner :: Word64 -> Word64 -> Word64 -> Word64 -> Word64
    -- TODO: more efficient impplementation
    inner !negInv 32 !_ !_ = negInv
    inner !negInv !i !s !t
      | even t = inner (negInv + s) (i + 1) (2 * s) ((t + m) `div` 2)
      | otherwise = inner negInv (i + 1) (2 * s) (t `div` 2)

-- | Retrieves the modulus \(m\).
umod :: Montgomery -> Word64
umod = mMontgomery

-- | Calculates \(a \cdot b \bmod m\).
mulMod :: Montgomery -> Word64 -> Word64 -> Word64
mulMod m a b = reduce m $ generate m a * generate m b

-- | Calculates \(a \cdot b \bmod m\).
mulModGenerated :: Montgomery -> Word64 -> Word64 -> Word64
mulModGenerated m a b = reduce m $ a * b

-- | Retrieves \( a \cdot b \bbmod m from a \cdot b\).
generate :: Montgomery -> Word64 -> Word64
generate m x = reduce m $ x * r2Montgomery m

-- | Retrieves \( a \cdot b \bbmod m from a \cdot b\). Prefer `reduce32`.
reduce :: Montgomery -> Word64 -> Word64
reduce Montgomery {mMontgomery, negInvMontgomery} x =
  let !x' = (x + fromIntegral (as32 x * negInvMontgomery) * mMontgomery) !>>. 32
   in if x' < mMontgomery then x' else x' - mMontgomery
  where
    -- TODO: makes sense??
    as32 :: Word64 -> Word32
    as32 = fromIntegral
