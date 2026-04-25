{-# OPTIONS_HADDOCK hide #-}

-- | Bit operations not in the `Data.Bits` module.
--
-- ==== __Example__
--
-- Ceiling functions:
--
-- >>> map bitCeil [0..4]
-- [1,1,2,4,4]
--
-- >>> map ceilingLog2 [0..4]
-- [0,0,1,2,2]
--
-- Flooring functions:
--
-- >>> map bitFloor [1..5]
-- [1,2,2,4,4]
--
-- >>> map floorLog2 [1..5]
-- [0,1,1,2,2]
--
-- @since 1.0.0.0
module AtCoder.Internal.Bit
  ( -- * Utilities
    bitCeil,
    ceilingLog2,
    bitFloor,
    floorLog2,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import Data.Bits (bit, countLeadingZeros)
import GHC.Stack (HasCallStack)

-- | \(O(1)\) Returns minimum power of two \(2^i\) s.t. \(2^i \geq n\).
--
-- @since 1.0.0.0
{-# INLINE bitCeil #-}
bitCeil :: Int -> Int
bitCeil = bit . ceilingLog2

-- | \(O(1)\) Returns minimum exponent \(i\) s.t. \(2^i \geq n\).
--
-- @since 1.6.0.0
{-# INLINE ceilingLog2 #-}
ceilingLog2 :: Int -> Int
ceilingLog2 x
  | x > 1 = 64 - countLeadingZeros (x - 1)
  | otherwise = 0

-- | \(O(1)\) Returns minimum power of two \(2^i\) s.t. \(2^i \leq n\).
--
-- @since 1.6.0.0
{-# INLINE bitFloor #-}
bitFloor :: (HasCallStack) => Int -> Int
bitFloor = bit . floorLog2

-- | \(O(1)\) Returns maximum exponent \(i\) s.t. \(2^i \leq n\).
--
-- ==== Constraints
-- - \(i \gt 0\), otherwise it results in an error.
--
-- @since 1.6.0.0
floorLog2 :: (HasCallStack) => Int -> Int
floorLog2 x =
  let !_ = ACIA.runtimeAssert (x > 0) $ "AtCoder.Internal.Bit.floorLog2: given non-positive value `" ++ show x ++ "`"
   in 63 - countLeadingZeros x
