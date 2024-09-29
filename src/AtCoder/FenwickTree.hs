{-# LANGUAGE RecordWildCards #-}

module AtCoder.FenwickTree (FenwickTree, new, add, sum) where

import Control.Exception (assert)
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bits
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Prelude hiding (sum)

data FenwickTree s a = FenwickTree
  { nFT :: {-# UNPACK #-} !Int,
    dataFT :: !(VUM.MVector s a)
  }

-- | \(O(n)\) Creates `FenwickTree`.
new :: (PrimMonad m, Num a, VU.Unbox a) => Int -> m (FenwickTree (PrimState m) a)
new nFT = do
  dataFT <- VUM.replicate nFT 0
  return FenwickTree {..}

-- | \(O(\log n)\) Calculates the sum in half-open range @[l, r)@.
add :: (PrimMonad m, Num a, VU.Unbox a) => FenwickTree (PrimState m) a -> Int -> a -> m ()
add FenwickTree {..} p0 x = do
  let !_ = assert (0 <= p0 && p0 < nFT) ()
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
sum :: (PrimMonad m, Num a, VU.Unbox a) => FenwickTree (PrimState m) a -> Int -> Int -> m a
sum ft@FenwickTree {..} l r = do
  let !_ = assert (0 <= l && l <= r && r <= nFT) ()
  xr <- prefixSum ft r
  xl <- prefixSum ft l
  return $! xr - xl
