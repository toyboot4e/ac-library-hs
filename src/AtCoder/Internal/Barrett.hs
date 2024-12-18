{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- | Fast modular multiplication by barrett reduction.
-- Reference: https://en.wikipedia.org/wiki/Barrett_reduction
--
-- ==== Example
-- >>> let bt = new32 10 -- mod 10
-- >>> umod bt
-- 10
-- >>> mulMod bt 7 7
-- 9
--
-- @since 1.0.0
module AtCoder.Internal.Barrett
  ( -- * Barrett
    Barrett,
    -- * Constructors
    new32,
    new64,
    -- * Accessors
    umod,
    -- * Barrett reduction
    mulMod,
  )
where

import Data.WideWord.Word128 (Word128 (..))
import Data.Word (Word32, Word64)

-- | Fast modular multiplication by barrett reduction.
-- Reference: https://en.wikipedia.org/wiki/Barrett_reduction
--
-- @since 1.0.0
data Barrett = Barrett
  { mBarrett :: {-# UNPACK #-} !Word32,
    imBarrett :: {-# UNPACK #-} !Word64
  }
  deriving
    ( -- | @since 1.0.0
      Eq,
      -- | @since 1.0.0
      Show
    )

-- | Creates barret reduction for modulus \(m\) from a `Word32` value.
--
-- @since 1.0.0
{-# INLINE new32 #-}
new32 :: Word32 -> Barrett
new32 m = Barrett m $ maxBound @Word64 `div` (fromIntegral m :: Word64) + 1

-- | Creates barret reduction for modulus \(m\) from a `Word64` value.
--
-- @since 1.0.0
{-# INLINE new64 #-}
new64 :: Word64 -> Barrett
new64 m = Barrett (fromIntegral m) $ maxBound @Word64 `div` m + 1

-- | Retrieves the modulus \(m\).
--
-- @since 1.0.0
{-# INLINE umod #-}
umod :: Barrett -> Word32
umod Barrett {mBarrett} = mBarrett

-- | Calculates \(a b \bmod m\).
--
-- @since 1.0.0
{-# INLINE mulMod #-}
mulMod :: Barrett -> Word64 -> Word64 -> Word64
mulMod Barrett {..} a b =
  let z :: Word64 = a * b
      x :: Word64 = word128Hi64 ((fromIntegral z :: Word128) * (fromIntegral imBarrett :: Word128))
      y :: Word64 = x * fromIntegral mBarrett
   in fromIntegral z - y + if fromIntegral z < y then fromIntegral mBarrett else 0
