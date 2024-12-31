{-# LANGUAGE RecordWildCards #-}

-- | A Fenwick tree, also known as binary indexed tree. Given an array of length \(n\), it processes
-- the following queries in \(O(\log n)\) time.
--
-- - Updating an element
-- - Calculating the sum of the elements of an interval
--
-- ==== __Example__
-- You can create a `FenwickTree` with `new`:
--
-- >>> import AtCoder.FenwickTree qualified as FT
-- >>> ft <- FT.new @_ @Int 4 -- [0, 0, 0, 0]
-- >>> FT.nFt ft
-- 4
--
-- It can perform point `add` and range `sum` in \(O(\log n)\) time:
--
-- >>> FT.add ft 0 3          -- [3, 0, 0, 0]
-- >>> FT.sum ft 0 3
-- 3
--
-- >>> FT.add ft 2 3          -- [3, 0, 3, 0]
-- >>> FT.sum ft 0 3
-- 6
--
-- You can create a `FenwickTree` with initial values using `build`:
--
-- >>> ft <- FT.build @_ @Int $ VU.fromList [3, 0, 3, 0]
-- >>> FT.add ft 1 2          -- [3, 2, 3, 0]
-- >>> FT.sum ft 0 3
-- 8
--
-- @since 1.0.0.0
module AtCoder.FenwickTree
  ( -- * Fenwick tree
    FenwickTree (nFt),

    -- * Constructors
    new,
    build,

    -- * Adding
    add,

    -- * Accessor
    sum,
    sumMaybe,
  )
where

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

-- | A Fenwick tree.
--
-- @since 1.0.0.0
data FenwickTree s a = FenwickTree
  { -- | 1.0.0 The number of vertices.
    --
    -- @since 1.0.0.0
    nFt :: {-# UNPACK #-} !Int,
    -- | The data storage.
    dataFt :: !(VUM.MVector s a)
  }

-- | Creates an array \([a_0, a_1, \cdots, a_{n-1}]\) of length \(n\). All the elements are
-- initialized to \(0\).
--
-- ==== Constraints
-- - \(0 \leq n\)
--
-- ==== Complexity
-- - \(O(n)\)
--
-- @since 1.0.0.0
{-# INLINE new #-}
new :: (HasCallStack, PrimMonad m, Num a, VU.Unbox a) => Int -> m (FenwickTree (PrimState m) a)
new nFt
  | nFt >= 0 = do
      dataFt <- VUM.replicate nFt 0
      pure FenwickTree {..}
  | otherwise = error $ "AtCoder.FenwickTree.new: given negative size `" ++ show nFt ++ "`"

-- | Creates `FenwickTree` with initial values.
--
-- ==== Complexity
-- - \(O(n)\)
--
-- @since 1.0.0.0
build :: (PrimMonad m, Num a, VU.Unbox a) => VU.Vector a -> m (FenwickTree (PrimState m) a)
{-# INLINE build #-}
build xs = do
  ft <- new $ VU.length xs
  VU.iforM_ xs $ add ft
  pure ft

-- | Adds \(x\) to \(p\)-th value of the array.
--
-- ==== Constraints
-- - \(0 \leq l \lt n\)
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- @since 1.0.0.0
{-# INLINE add #-}
add :: (HasCallStack, PrimMonad m, Num a, VU.Unbox a) => FenwickTree (PrimState m) a -> Int -> a -> m ()
add FenwickTree {..} p0 x = do
  let !_ = ACIA.checkIndex "AtCoder.FenwickTree.add" p0 nFt
  let p1 = p0 + 1
  flip fix p1 $ \loop p -> do
    when (p <= nFt) $ do
      VGM.modify dataFt (+ x) (p - 1)
      loop $! p + (p .&. (-p))

-- | \(O(\log n)\) Calculates the sum in a half-open interval @[0, r)@.
--
-- @since 1.0.0.0
{-# INLINE prefixSum #-}
prefixSum :: (PrimMonad m, Num a, VU.Unbox a) => FenwickTree (PrimState m) a -> Int -> m a
prefixSum FenwickTree {..} = inner 0
  where
    inner !acc !r
      | r <= 0 = pure acc
      | otherwise = do
          dx <- VGM.read dataFt (r - 1)
          inner (acc + dx) (r - r .&. (-r))

-- | Calculates the sum in a half-open interval \([l, r)\).
--
-- ==== Constraints
-- - \(0 \leq l \leq r \leq n\)
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- @since 1.0.0.0
{-# INLINE sum #-}
sum :: (HasCallStack, PrimMonad m, Num a, VU.Unbox a) => FenwickTree (PrimState m) a -> Int -> Int -> m a
sum ft@FenwickTree {nFt} l r
  | not (ACIA.testInterval l r nFt) = ACIA.errorInterval "AtCoder.FenwickTree.sum" l r nFt
  | otherwise = unsafeSum ft l r

-- | Total variant of `sum`. Calculates the sum in a half-open interval \([l, r)\). It returns
-- `Nothing` if the interval is invalid.
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- @since 1.0.0.0
{-# INLINE sumMaybe #-}
sumMaybe :: (HasCallStack, PrimMonad m, Num a, VU.Unbox a) => FenwickTree (PrimState m) a -> Int -> Int -> m (Maybe a)
sumMaybe ft@FenwickTree {nFt} l r
  | not (ACIA.testInterval l r nFt) = pure Nothing
  | otherwise = Just <$> unsafeSum ft l r

-- | Internal implementation of `sum`.
{-# INLINE unsafeSum #-}
unsafeSum :: (HasCallStack, PrimMonad m, Num a, VU.Unbox a) => FenwickTree (PrimState m) a -> Int -> Int -> m a
unsafeSum ft l r = do
  xr <- prefixSum ft r
  xl <- prefixSum ft l
  pure $! xr - xl
