{-# OPTIONS_HADDOCK hide #-}

-- | Bit operations not in the `Data.Bits` module.
--
-- ==== Example
-- >>> bitCeil 0
-- 1
-- >>> bitCeil 1
-- 1
-- >>> bitCeil 2
-- 2
-- >>> bitCeil 3
-- 4
-- >>> bitCeil 4
-- 4
--
-- @since 1.0.0
module AtCoder.Internal.Bit
  ( -- * Utilities
    bitCeil,
  )
where

-- TODO: faster implmentation

-- | \(O(w)\) Returns minimum \(2^i s.t. 2^i \geq n\).
--
-- @since 1.0.0
{-# INLINE bitCeil #-}
bitCeil :: Int -> Int
bitCeil n = inner 1
  where
    inner x
      | x >= n = x
      | otherwise = inner $ 2 * x

-- countTrailingZeros from Data.Bits
