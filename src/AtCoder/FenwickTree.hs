{-# LANGUAGE RecordWildCards #-}

-- | A Fenwick tree, also known as binary indexed tree. Given an array of length \(n\), it processes
-- the following queries in \(O(\log n)\) time.
--
-- - Updating an element
-- - Calculating the sum of the elements of an interval
--
-- ==== __Example__
-- Create a `FenwickTree` with `new`:
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
-- Create a `FenwickTree` with initial values using `build`:
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

    -- * Accessors
    sum,
    sumMaybe,

    -- * Bisection methods
    maxRight,
    maxRightM,
    minLeft,
    minLeftM,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
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

-- | (Extra API) Applies a binary search on the Fenwick tree. It returns an index \(r\) that
-- satisfies both of the following.
--
-- - \(r = l\) or \(f(a[l] + a[l + 1] + ... + a[r - 1])\) returns `True`.
-- - \(r = n\) or \(f(a[l] + a[l + 1] + ... + a[r]))\) returns `False`.
--
-- If \(f\) is monotone, this is the maximum \(r\) that satisfies
-- \(f(a[l] + a[l + 1] + ... + a[r - 1])\).
--
-- ==== Constraints
-- - if \(f\) is called with the same argument, it returns the same value, i.e., \(f\) has no side effect.
-- - \(f(0)\) returns `True`
-- - \(0 \leq l \leq n\)
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- @since 1.2.2.0
{-# INLINE maxRight #-}
maxRight ::
  (HasCallStack, PrimMonad m, Num a, VU.Unbox a) =>
  -- | The Fenwick tree
  FenwickTree (PrimState m) a ->
  -- | \(l\)
  Int ->
  -- | \(p\): user predicate
  (a -> Bool) ->
  -- | \(r\): \(p\) holds for \([l, r)\)
  m Int
maxRight ft l0 f = maxRightM ft l0 (pure . f)

-- | (Extra API) Monadic variant of `maxRight`.
--
-- ==== Constraints
-- - if \(f\) is called with the same argument, it returns the same value, i.e., \(f\) has no side effect.
-- - \(f(0)\) returns `True`
-- - \(0 \leq l \leq n\)
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- @since 1.2.2.0
{-# INLINEABLE maxRightM #-}
maxRightM ::
  forall m a.
  (HasCallStack, PrimMonad m, Num a, VU.Unbox a) =>
  -- | The Fenwick tree
  FenwickTree (PrimState m) a ->
  -- | \(l\)
  Int ->
  -- | \(p\): user predicate
  (a -> m Bool) ->
  -- | \(r\): \(p\) holds for \([l, r)\)
  m Int
maxRightM FenwickTree {..} l0 f = do
  b0 <- f 0
  let !_ = ACIA.runtimeAssert b0 "AtCoder.FenwickTree.maxRightM: `f 0` must return `True`"

  let inner i !s
        | odd i = do
            ds <- stToPrim $ VGM.read dataFt (i - 1)
            inner (i - 1) $! s - ds
        | i == 0 = pure (i, 64 - countLeadingZeros nFt, s)
        | i + bit k > nFt = pure (i, k, s)
        | otherwise = do
            t <- stToPrim $ (s +) <$> VGM.read dataFt (i + bit k - 1)
            b <- f t
            if not b
              then pure (i, k, s)
              else do
                di <- stToPrim $ VGM.read dataFt (i - 1)
                inner (i - i .&. (-i)) $! s - di
        where
          k = countTrailingZeros i - 1

  -- we could start from an arbitrary l, but the API is limited to one
  (!i0, !k0, !s0) <- inner l0 (0 :: a)

  let inner2 i k_ !s
        | k_ == 0 = pure i
        | i + bit k - 1 < VGM.length dataFt = do
            t <- stToPrim $ (s +) <$> VGM.read dataFt (i + bit k - 1)
            b <- f t
            if b
              then inner2 (i + bit k) k t
              else inner2 i k s
        | otherwise = inner2 i k s
        where
          k = k_ - 1

  inner2 i0 k0 s0

-- | Applies a binary search on the Fenwick tree. It returns an index \(l\) that satisfies both of
-- the following.
--
-- - \(l = r\) or \(f(a[l] + a[l + 1] + ... + a[r - 1])\) returns `True`.
-- - \(l = 0\) or \(f(a[l - 1] + a[l] + ... + a[r - 1])\) returns `False`.
--
-- If \(f\) is monotone, this is the minimum \(l\) that satisfies
-- \(f(a[l] + a[l + 1] + ... + a[r - 1])\).
--
-- ==== Constraints
--
-- - if \(f\) is called with the same argument, it returns the same value, i.e., \(f\) has no side
--   effect.
-- - \(f(0)\) returns `True`
-- - \(0 \leq r \leq n\)
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- @since 1.2.2.0
{-# INLINE minLeft #-}
minLeft ::
  (HasCallStack, PrimMonad m, Num a, VU.Unbox a) =>
  -- | The Fenwick tree
  FenwickTree (PrimState m) a ->
  -- | \(r\)
  Int ->
  -- | \(p\): user prediate
  (a -> Bool) ->
  -- | \(l\): \(p\) holds for \([l, r)\)
  m Int
minLeft ft r0 f = minLeftM ft r0 (pure . f)

-- | (Extra API) Monadic variant of `minLeft`.
--
-- ==== Constraints
-- - if \(f\) is called with the same argument, it returns the same value, i.e., \(f\) has no side effect.
-- - \(f(0)\) returns `True`
-- - \(0 \leq l \leq n\)
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- @since 1.2.2.0
{-# INLINEABLE minLeftM #-}
minLeftM ::
  forall m a.
  (HasCallStack, PrimMonad m, Num a, VU.Unbox a) =>
  -- | The Fenwick tree
  FenwickTree (PrimState m) a ->
  -- | \(r\)
  Int ->
  -- | \(p\): user prediate
  (a -> m Bool) ->
  -- | \(l\): \(p\) holds for \([l, r)\)
  m Int
minLeftM FenwickTree {..} r0 f = do
  b0 <- f 0
  let !_ = ACIA.runtimeAssert b0 "AtCoder.FenwickTree.minLeftM: `f 0` must return `True`"

  let inner i k !s
        | i <= 0 = pure (i, k, s)
        | otherwise = do
            b <- f s
            if not b
              then pure (i, k, s)
              else do
                s' <- stToPrim $ (s +) <$> VGM.read dataFt (i - 1)
                inner (i - i .&. (-i)) (countTrailingZeros i) s'

  -- we could start from an arbitrary r, but the API is limited to n
  (!i0, !k0, !s0) <- inner r0 (0 :: Int) (0 :: a)

  b0_ <- f s0
  if b0_
    then do
      let !_ = ACIA.runtimeAssert (i0 == 0) "AtCoder.FenwickTree.minLeftM: implementation error"
      pure 0
    else do
      let inner2 i k_ !s
            | k_ == 0 = pure i
            | otherwise = do
                let k = k_ - 1
                t <- stToPrim $ (s -) <$> VGM.read dataFt (i + bit k - 1)
                b <- f t
                if b
                  then inner2 i k s
                  else inner2 (i + bit k) k t

      (+ 1) <$> inner2 i0 k0 s0
