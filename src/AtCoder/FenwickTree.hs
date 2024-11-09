{-# LANGUAGE RecordWildCards #-}

-- | Fenwick tree.
module AtCoder.FenwickTree (FenwickTree, new, build, add, sum) where

import AtCoder.Internal.Assert qualified as ACIA
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
    nFt :: {-# UNPACK #-} !Int,
    -- | The data storage.
    dataFt :: !(VUM.MVector s a)
  }

-- | \(O(n)\) Creates `FenwickTree`.
new :: (HasCallStack, Num a, VU.Unbox a, PrimMonad m) => Int -> m (FenwickTree (PrimState m) a)
new nFt
  | nFt >= 0 = do
    dataFt <- VUM.replicate nFt 0
    return FenwickTree {..}
  | otherwise = error $ "AtCoder.FenwickTree.new: given negative size `" ++ show nFt ++ "`"

-- | \(O(n)\) Creates `FenwickTree` from a vector of monoids. Mostly a shorthand.
build :: (Num a, VU.Unbox a, PrimMonad m) => VU.Vector a -> m (FenwickTree (PrimState m) a)
build xs = do
  ft <- new $ VU.length xs
  VU.iforM_ xs $ add ft
  return ft

-- | \(O(\log n)\) Calculates the sum in half-open range @[l, r)@.
add :: (HasCallStack, PrimMonad m, Num a, VU.Unbox a) => FenwickTree (PrimState m) a -> Int -> a -> m ()
add FenwickTree {..} p0 x = do
  let !_ = ACIA.checkIndex "AtCoder.FenwickTree.add" p0 nFt
  let p1 = p0 + 1
  flip fix p1 $ \loop p -> do
    when (p <= nFt) $ do
      -- FIXME: to unsigned?
      VGM.modify dataFt (+ x) (p - 1)
      loop $! p + (p .&. (-p))

-- | \(O(\log n)\) Calculates the sum in half-open range @[0, r)@.
prefixSum :: (PrimMonad m, Num a, VU.Unbox a) => FenwickTree (PrimState m) a -> Int -> m a
prefixSum FenwickTree {..} = inner 0
  where
    inner !acc !r
      | r <= 0 = return acc
      | otherwise = do
          dx <- VGM.read dataFt (r - 1)
          inner (acc + dx) (r - r .&. (-r))

-- | \(O(\log n)\) Calculates the sum in half-open range @[l, r)@.
sum :: (HasCallStack, PrimMonad m, Num a, VU.Unbox a) => FenwickTree (PrimState m) a -> Int -> Int -> m a
sum ft@FenwickTree {..} l r = do
  let !_ = ACIA.checkInterval "AtCoder.FenwickTree.sum" l r nFt
  xr <- prefixSum ft r
  xl <- prefixSum ft l
  return $! xr - xl
