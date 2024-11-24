{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module AtCoder.Internal.Barrett (Barrett, new, umod, mul) where

-- TODO: Use MagicHash?
-- TODO: Does it make it really faster? Or even slower? Benchmark!

import Data.WideWord.Word128 (Word128 (..))
import Data.Word (Word32, Word64)

-- | Fast modular multiplication by barrett reduction.
--  Reference: https://en.wikipedia.org/wiki/Barrett_reduction
data Barrett = Barrett
  { mBarrett :: {-# UNPACK #-} !Word32,
    imBarrett :: {-# UNPACK #-} !Word64
  }

new :: Word32 -> Barrett
new m =
  Barrett m $
    maxBound @Word64 `div` fromIntegral (fromIntegral m :: Word64) + 1

umod :: Barrett -> Word32
umod Barrett {mBarrett} = mBarrett

mul :: Barrett -> Word32 -> Word32 -> Word32
mul Barrett {..} a b =
  let z :: Word64 = fromIntegral a * fromIntegral b
      x :: Word64 = word128Hi64 ((fromIntegral z :: Word128) * fromIntegral imBarrett)
      y :: Word64 = x * fromIntegral mBarrett
   in fromIntegral $ fromIntegral z - y + if fromIntegral z < y then fromIntegral mBarrett else 0
