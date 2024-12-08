{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- | Fast modular multiplication by barrett reduction.
--
-- = Example
-- >>> let bt = new 10 -- mod 10
-- >>> umod bt
-- 10
-- >>> mul bt 7 7
-- 9
module BenchLib.MulMod.Mongomery (Mongomery (mMongomery), new, umod, mulMod, generate, reduce) where

-- FIXME: test
-- TODO: Use MagicHash?

import Data.Bits (bit, (!>>.))
import Data.Word (Word64)

-- | Fast modular multiplication by Mongomery multiplication. The modulus value has to be odd
-- to be fast.
data Mongomery = Mongomery
  { mMongomery :: {-# UNPACK #-} !Word64,
    -- | MOD * NEG_INV % (2^32) == (2^32) - 1;
    negInvMongomery :: {-# UNPACK #-} !Word64,
    -- | R2 == (2^64) % MOD;
    r2Mongomery :: {-# UNPACK #-} !Word64
  }

-- | Creates `Mongomery` for modulus @m@.
new :: Word64 -> Mongomery
new m =
  let !negInv = inner 0 0 1 0
      !r2 = bit 32 `rem` m
      !r2' = r2 * r2 `rem` m
   in Mongomery m negInv r2'
  where
    inner :: Word64 -> Word64 -> Word64 -> Word64 -> Word64
    inner !negInv 32 !_ !_ = negInv
    inner !negInv !i !s !t
      | even t = inner (negInv + s) (i + 1) (2 * s) ((t + m) `div` 2)
      | otherwise = inner negInv (i + 1) (2 * s) (t `div` 2)

-- | Retrieves the modulus \(m\).
umod :: Mongomery -> Word64 -> Word64 -> Word64
umod m a b = reduce m $ reduce m a * reduce m b

-- | Calculates \(a \cdot b \bmod m\).
mulMod :: Mongomery -> Word64 -> Word64 -> Word64
mulMod m a b = reduce m $ generate m a * generate m b

-- | Retrieves \( a \cdot b \bbmod m from a \cdot b\).
generate :: Mongomery -> Word64 -> Word64
generate m x = reduce m $ x * r2Mongomery m

-- | Retrieves \( a \cdot b \bbmod m from a \cdot b\).
reduce :: Mongomery -> Word64 -> Word64
reduce Mongomery {mMongomery, negInvMongomery} x =
  let !x' = (x + x * negInvMongomery * mMongomery) !>>. 32
   in if x' < mMongomery then x' else x' - mMongomery
