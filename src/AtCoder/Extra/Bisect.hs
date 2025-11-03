{-# LANGUAGE LambdaCase #-}

-- | Bisection methods and binary search functions.
--
-- ==== __Example__
-- Perform index compression with `lowerBound`:
--
-- >>> import AtCoder.Extra.Bisect
-- >>> import Data.Vector.Algorithms.Intro qualified as VAI
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let xs = VU.fromList ([0, 20, 10, 40, 30] :: [Int])
-- >>> let dict = VU.uniq $ VU.modify VAI.sort xs
-- >>> VU.map (lowerBound dict) xs
-- [0,2,1,4,3]
--
-- @since 1.3.0.0
module AtCoder.Extra.Bisect
  ( -- * C++-like binary searches
    lowerBound,
    lowerBoundIn,
    upperBound,
    upperBoundIn,

    -- * Generic bisection methods
    maxRight,
    maxRightM,
    minLeft,
    minLeftM,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import Data.Functor.Identity
import Data.Vector.Generic qualified as VG
import GHC.Stack (HasCallStack)

-- | \(O(\log n)\) Returns the maximum \(r_{\mathrm{max}}\) where \(x_i \lt x_{r_{\mathrm{max}}}\)
-- holds for \(i \in [0, r_{\mathrm{max}})\).
--
-- ==== __Example__
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let xs = VU.fromList [1, 1, 2, 2, 4, 4]
-- >>> lowerBound xs 1
-- 0
--
-- >>> lowerBound xs 2
-- 2
--
-- >>> lowerBound xs 3
-- 4
--
-- >>> lowerBound xs 4
-- 4
--
-- >>> lowerBound xs 5
-- 6
--
-- @since 1.3.0.0
{-# INLINE lowerBound #-}
lowerBound :: (HasCallStack, VG.Vector v a, Ord a) => v a -> a -> Int
lowerBound vec = lowerBoundIn 0 (VG.length vec) vec

-- | \(O(\log n)\) Returns the maximum \(r_{\mathrm{max}}\) where \(x_i \lt x_{r_{\mathrm{max}}}\)
-- holds for \(i \in [l, r_{\mathrm{max}})\) ( \(r_{\mathrm{max}} \le r\) ).
--
-- ==== Constraints
-- - \(0 \le l \le r \le n\)
--
-- ==== __Example__
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let xs = VU.fromList [10, 10, 20, 20, 40, 40]
-- >>> --                            *---*---*
-- >>> lowerBoundIn 2 5 xs 10
-- 2
--
-- >>> lowerBoundIn 2 5 xs 20
-- 2
--
-- >>> lowerBoundIn 2 5 xs 30
-- 4
--
-- >>> lowerBoundIn 2 5 xs 40
-- 4
--
-- >>> lowerBoundIn 2 5 xs 50
-- 5
--
-- @since 1.3.0.0
{-# INLINE lowerBoundIn #-}
lowerBoundIn :: (HasCallStack, VG.Vector v a, Ord a) => Int -> Int -> v a -> a -> Int
lowerBoundIn l r vec target = maxRight l r $ \i -> vec VG.! (i - 1) < target
  where
    !_ = ACIA.checkIntervalBounded "AtCoder.Extra.Bisect.lowerBoundIn" l r $ VG.length vec

-- | \(O(\log n)\) Returns the maximum \(r_{\mathrm{max}}\) where \(x_i \le x_{r_{\mathrm{max}}}\)
-- holds for \(i \in [0, r_{\mathrm{max}})\).
--
-- ==== __Example__
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let xs = VU.fromList [10, 10, 20, 20, 40, 40]
-- >>> upperBound xs 0
-- 0
--
-- >>> upperBound xs 10
-- 2
--
-- >>> upperBound xs 20
-- 4
--
-- >>> upperBound xs 30
-- 4
--
-- >>> upperBound xs 39
-- 4
--
-- >>> upperBound xs 40
-- 6
--
-- @since 1.3.0.0
{-# INLINE upperBound #-}
upperBound :: (HasCallStack, VG.Vector v a, Ord a) => v a -> a -> Int
upperBound vec = upperBoundIn 0 (VG.length vec) vec

-- | \(O(\log n)\) Returns the maximum \(r_{\mathrm{max}}\) where \(x_i \le x_{r_{\mathrm{max}}}\)
-- holds for \(i \in [l, r_{\mathrm{max}})\) ( \(r_{\mathrm{max}} \le r\) ).
--
-- ==== Constraints
-- - \(0 \le l \le r \le n\)
--
-- ==== __Example__
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let xs = VU.fromList [10, 10, 20, 20, 40, 40]
-- >>> --                            *---*---*
-- >>> upperBoundIn 2 5 xs 0
-- 2
--
-- >>> upperBoundIn 2 5 xs 10
-- 2
--
-- >>> upperBoundIn 2 5 xs 20
-- 4
--
-- >>> upperBoundIn 2 5 xs 30
-- 4
--
-- >>> upperBoundIn 2 5 xs 40
-- 5
--
-- >>> upperBoundIn 2 5 xs 50
-- 5
--
-- @since 1.3.0.0
{-# INLINE upperBoundIn #-}
upperBoundIn :: (HasCallStack, VG.Vector v a, Ord a) => Int -> Int -> v a -> a -> Int
upperBoundIn l r vec target = maxRight l r $ \i -> vec VG.! (i - 1) <= target
  where
    !_ = ACIA.checkIntervalBounded "AtCoder.Extra.Bisect.upperBoundIn" l r $ VG.length vec

-- | \(O(\log n)\) Applies the bisection method on a half-open interval \([l, r)\) and returns the
-- right boundary point \(r_{\mathrm{max}}\), where \(p[l, i)\) holds for
-- \(i \in [l, r_{\mathrm{max}}]\).
--
-- ==== __Example__
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let xs = VU.fromList [10, 10, 20, 20, 30, 30]
-- >>> let n = VU.length xs
-- >>> maxRight 0 n (\i -> xs VU.! (i - 1) <= 20)
-- 4
--
-- >>> maxRight 0 n (\i -> xs VU.! (i - 1) <= 0)
-- 0
--
-- >>> maxRight 0 n (\i -> xs VU.! (i - 1) <= 100)
-- 6
--
-- >>> maxRight 0 3 (\i -> xs VU.! (i - 1) <= 20)
-- 3
--
-- @since 1.3.0.0
{-# INLINE maxRight #-}
maxRight ::
  (HasCallStack) =>
  -- | \(l\)
  Int ->
  -- | \(r\)
  Int ->
  -- | \(p\): user predicate that works on \([l, i)\)
  (Int -> Bool) ->
  -- | Maximum \(r_{\mathrm{max}} (r_{\mathrm{max}} \le r)\) where \(p[l, i)\) holds for
  -- \(i \in [l, r_{\mathrm{max}}]\).
  Int
maxRight l r p = runIdentity $ maxRightM l r (pure . p)

-- | \(O(\log n)\) Monadic variant of `maxRight`.
--
-- @since 1.3.0.0
{-# INLINE maxRightM #-}
maxRightM :: (HasCallStack, Monad m) => Int -> Int -> (Int -> m Bool) -> m Int
maxRightM l0 r0 = bisectImpl l0 (r0 + 1)
  where
    !_ = ACIA.checkInterval "AtCoder.Extra.Bisect.maxRightM" l0 r0

-- | \(O(\log n)\) Applies the bisection method on a half-open interval \([l, r)\) and returns the
-- right boundary point \(l_{\mathrm{min}}\), where \(p[i, r)\) holds for
-- \(i \in [l_{\mathrm{min}}, r]\).
--
-- ==== __Example__
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let xs = VU.fromList [10, 10, 20, 20, 30, 30]
-- >>> let n = VU.length xs
-- >>> minLeft 0 n (\i -> xs VU.! i >= 20)
-- 2
--
-- >>> minLeft 0 n (\i -> xs VU.! i >= 0)
-- 0
--
-- >>> minLeft 0 n (\i -> xs VU.! i >= 100)
-- 6
--
-- @since 1.3.0.0
{-# INLINE minLeft #-}
minLeft ::
  (HasCallStack) =>
  -- | \(l\)
  Int ->
  -- | \(r\)
  Int ->
  -- | \(p\): user predicate that works on \([i, r)\)
  (Int -> Bool) ->
  -- | Minimum \(l_{\mathrm{min}}(l_{\mathrm{min}} \ge l)\) where \(p[i, r)\) holds for
  -- \(i \in [l_{\mathrm{min}}, r]\)
  Int
minLeft l r p = runIdentity $ minLeftM l r (pure . p)

-- | \(O(\log n)\) Monadic variant of `minLeft`.
--
-- @since 1.3.0.0
{-# INLINE minLeftM #-}
minLeftM :: (HasCallStack, Monad m) => Int -> Int -> (Int -> m Bool) -> m Int
minLeftM l r = bisectImpl r (l - 1)
  where
    !_ = ACIA.checkInterval "AtCoder.Extra.Bisect.minLeftM" l r

-- | Takes [l, r + 1) on `maxRight` or [r, l - 1) on `minLeft`.
{-# INLINE bisectImpl #-}
bisectImpl :: (HasCallStack, Monad m) => Int -> Int -> (Int -> m Bool) -> m Int
bisectImpl l0 r0 p = inner l0 r0
  where
    inner l r
      | abs (r - l) <= 1 = pure l
      | otherwise =
          p mid >>= \case
            True -> inner mid r
            False -> inner l mid
      where
        mid = (l + r) `div` 2
