{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- | Fast modular multiplication by barrett reduction.
module BenchLib.MulMod.BarrettWideWord
  ( Barrett,
    new32,
    new64,
    umod,
    mulMod,
  )
where

-- TODO: Use MagicHash?

import Data.WideWord.Word128 (Word128 (..))
import Data.Word (Word32, Word64)

-- | Fast modular multiplication by barrett reduction.
--  Reference: https://en.wikipedia.org/wiki/Barrett_reduction
data Barrett = Barrett
  { -- TODO: should we have it as `Word64`?
    mBarrett :: {-# UNPACK #-} !Word32,
    imBarrett :: {-# UNPACK #-} !Word64
  }

-- | Creates a `Barrett` for modulus value \(m\) of type `Word32`.
new32 :: Word32 -> Barrett
new32 m = Barrett m $ maxBound @Word64 `div` (fromIntegral m :: Word64) + 1

-- | Creates a `Barrett` for modulus value \(m\) of type `Word64`.
new64 :: Word64 -> Barrett
new64 m = Barrett (fromIntegral m) $ maxBound @Word64 `div` m + 1

-- | Retrieves the modulus \(m\).
umod :: Barrett -> Word32
umod Barrett {mBarrett} = mBarrett

-- | Calculates \(a \cdot b \bmod m\).
mulMod :: Barrett -> Word64 -> Word64 -> Word64
mulMod Barrett {..} a b =
  let z :: Word64 = a * b
      x :: Word64 = word128Hi64 ((fromIntegral z :: Word128) * (fromIntegral imBarrett :: Word128))
      y :: Word64 = x * fromIntegral mBarrett
   in fromIntegral z - y + if fromIntegral z < y then fromIntegral mBarrett else 0
