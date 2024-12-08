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
module BenchLib.MulMod.BarrettWideWord (Barrett, new, umod, mulMod) where

-- TODO: Use MagicHash?

import Data.WideWord.Word128 (Word128 (..))
import Data.Word (Word32, Word64)

-- | Fast modular multiplication by barrett reduction.
--  Reference: https://en.wikipedia.org/wiki/Barrett_reduction
data Barrett = Barrett
  { mBarrett :: {-# UNPACK #-} !Word32,
    imBarrett :: {-# UNPACK #-} !Word64
  }

-- | Creates barret reduction for modulus \(m\).
new :: Word32 -> Barrett
new m =
  Barrett m $
    maxBound @Word64 `div` fromIntegral (fromIntegral m :: Word64) + 1

-- | Retrieves the modulus \(m\).
umod :: Barrett -> Word32
umod Barrett {mBarrett} = mBarrett

-- | Calculates \(a \cdot b \bmod m\).
mulMod :: Barrett -> Word32 -> Word32 -> Word32
mulMod Barrett {..} a b =
  let z :: Word64 = fromIntegral a * fromIntegral b
      x :: Word64 = word128Hi64 ((fromIntegral z :: Word128) * fromIntegral imBarrett)
      y :: Word64 = x * fromIntegral mBarrett
   in fromIntegral $ fromIntegral z - y + if fromIntegral z < y then fromIntegral mBarrett else 0
