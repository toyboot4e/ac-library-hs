{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- | Fast modular multiplication by barrett reduction.
-- Reference: https://en.wikipedia.org/wiki/Barrett_reduction
--
-- = Example
-- >>> let bt = new 10 -- mod 10
-- >>> umod bt
-- 10
-- >>> mul bt 7 7
-- 9
module BenchLib.MulMod.Barrett64 (Barrett (mBarrett), new, umod, mulMod) where

-- FIXME: test
-- TODO: Use MagicHash?

import Data.Bits (bit, complement, (.&.), (.>>.))
import Data.Word (Word64)

-- | Fast modular multiplication by barrett reduction.
--  Reference: https://en.wikipedia.org/wiki/Barrett_reduction
data Barrett = Barrett
  { mBarrett :: {-# UNPACK #-} !Word64,
    mhBarrett :: {-# UNPACK #-} !Word64,
    mlBarrett :: {-# UNPACK #-} !Word64
  }

-- | Creates barret reduction for modulus \(m\).
new :: Word64 -> Barrett
new m0 =
  let !m = complement 0 `div` m0
      !m' = if m * m0 + m0 == 0 then m + 1 else m
      !mh = m' .>>. 32
      !ml = m' .&. (bit 32 - 1)
   in Barrett m0 mh ml

-- | Retrieves the modulus \(m\).
umod :: Barrett -> Word64
umod = mBarrett

-- | Calculates \(a \cdot b \bmod m\).
mulMod :: Barrett -> Word64 -> Word64 -> Word64
mulMod bt a b = reduce bt (a * b)

-- | Retrieves \( a \cdot b \bbmod m from a \cdot b\).
reduce :: Barrett -> Word64 -> Word64
reduce Barrett {..} x =
  let !z = (x .&. (bit 32 - 1)) * mlBarrett
      !z' = (x .&. (bit 32 - 1)) * mhBarrett + (x .>>. 32) * mlBarrett + (z .>>. 32)
      !z'' = (x .>>. 32) * mhBarrett + (z' .>>. 32)
      !x' = x - z'' * mBarrett
   in if x' < mBarrett then x' else x' - mBarrett
