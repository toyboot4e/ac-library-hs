{-# LANGUAGE RecordWildCards #-}

-- | Fenwick tree.
module AtCoder.FenwickTree (FenwickTree, new, add, sum) where

import AtCoder.Internal.Assert (runtimeAssert)
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bits
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)
import Prelude hiding (sum)

-- | Fenwick tree.
data FenwickTree s a = FenwickTree
  { -- | The number of vertices.
    nFT :: {-# UNPACK #-} !Int,
    -- | The data storage.
    dataFT :: !(VUM.MVector s a)
  }

-- | \(O(n)\) Creates `FenwickTree`.
new :: (Num a, VU.Unbox a, PrimMonad m) => Int -> m (FenwickTree (PrimState m) a)
new nFT = do
  dataFT <- VUM.replicate nFT 0
  return FenwickTree {..}

-- | \(O(\log n)\) Calculates the sum in half-open range @[l, r)@.
add :: (HasCallStack, PrimMonad m, Num a, VU.Unbox a) => FenwickTree (PrimState m) a -> Int -> a -> m ()
add FenwickTree {..} p0 x = do
  let !_ = runtimeAssert (0 <= p0 && p0 < nFT) "add: vertex out of bounds"
  let p1 = p0 + 1
  flip fix p1 $ \loop p -> do
    when (p <= nFT) $ do
      -- FIXME: to unsigned?
      VGM.modify dataFT (+ x) (p - 1)
      loop $! p + (p .&. (-p))

-- | \(O(\log n)\) Calculates the sum in half-open range @[0, r)@.
prefixSum :: (PrimMonad m, Num a, VU.Unbox a) => FenwickTree (PrimState m) a -> Int -> m a
prefixSum FenwickTree {..} = inner 0
  where
    inner !acc !r
      | r <= 0 = return acc
      | otherwise = do
          dx <- VGM.read dataFT (r - 1)
          inner (acc + dx) (r - r .&. (-r))

-- | \(O(\log n)\) Calculates the sum in half-open range @[l, r)@.
sum :: (HasCallStack, PrimMonad m, Num a, VU.Unbox a) => FenwickTree (PrimState m) a -> Int -> Int -> m a
sum ft@FenwickTree {..} l r = do
  let !_ = runtimeAssert (0 <= l && l <= r && r <= nFT) "sum: invalid vertices"
  xr <- prefixSum ft r
  xl <- prefixSum ft l
  return $! xr - xl
