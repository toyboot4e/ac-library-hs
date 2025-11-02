{-# LANGUAGE LambdaCase #-}

-- | Bisection methods and binary search functions.
--
-- __Known bug__: `maxRight` and their variants have a bug ([#140](https://github.com/toyboot4e/ac-library-hs/issues/140)),
-- so don't use them.
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

    --
    -- __Known bug_:_ `maxRight` and their variants have a bug ([#140](https://github.com/toyboot4e/ac-library-hs/issues/140)),
    -- so don't use them.
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

-- | \(O(\log n)\) Returns the maximum \(r\) where \(x_i \lt x_{ref}\) holds for \(i \in [0, r)\).
--
-- @
-- Y Y Y Y Y N N N N N      Y: x_i < x_ref
-- --------- *---------> x  N: not Y
--           R              R: the right boundary point returned
-- @
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

-- | \(O(\log n)\) Computes the `lowerBound` for a slice of a vector within the interval \([l, r)\).
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
lowerBoundIn l r vec target = maxRight l r $ \i -> vec VG.! i < target
  where
    !_ = ACIA.checkIntervalBounded "AtCoder.Extra.Bisect.lowerBoundIn" l r $ VG.length vec

-- | \(O(\log n)\) Returns the maximum \(r\) where \(x_i \le x_{ref}\) holds for \(i \in [0, r)\).
--
-- @
-- Y Y Y Y Y N N N N N      Y: x_i <= x_ref
-- --------- *---------> x  N: not Y
--           R              R: the right boundary point returned
-- @
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

-- | \(O(\log n)\) Computes the `upperBound` for a slice of a vector within the interval \([l, r)\).
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
upperBoundIn l r vec target = maxRight l r $ \i -> vec VG.! i <= target
  where
    !_ = ACIA.checkIntervalBounded "AtCoder.Extra.Bisect.upperBoundIn" l r $ VG.length vec

-- | \(O(\log n)\) Applies the bisection method on a half-open interval \([l, r)\) and returns the
-- right boundary point.
--
-- @
-- Y Y Y Y Y N N N N N      Y: p(i) returns `true`
-- --------- *---------> x  N: not Y
--           R              R: the right boundary point returned
-- @
--
-- __Known bug__: user function \(p\) takes __closed intervals__ \([l, r]\).
--
-- ==== __Example__
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let xs = VU.fromList [10, 10, 20, 20, 30, 30]
-- >>> let n = VU.length xs
-- >>> maxRight 0 n ((<= 20) . (xs VU.!))
-- 4
--
-- >>> maxRight 0 n ((<= 0) . (xs VU.!))
-- 0
--
-- >>> maxRight 0 n ((<= 100) . (xs VU.!))
-- 6
--
-- >>> maxRight 0 3 ((<= 20) . (xs VU.!))
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
  -- | \(p\)
  (Int -> Bool) ->
  -- | Maximum \(r' (r' \le r)\) where \(p(i)\) holds for \(i \in [l, r')\).
  Int
maxRight l r p = runIdentity $ maxRightM l r (pure . p)

-- | \(O(\log n)\) Monadic variant of `maxRight`.
--
-- __Known bug__: user function \(p\) takes __closed intervals__ \([l, r]\).
--
-- @since 1.3.0.0
{-# INLINE maxRightM #-}
maxRightM :: (HasCallStack, Monad m) => Int -> Int -> (Int -> m Bool) -> m Int
maxRightM l0 r0 p = bisectImpl (l0 - 1) r0 p
  where
    !_ = ACIA.checkInterval "AtCoder.Extra.Bisect.maxRightM" l0 r0

-- | \(O(\log n)\) Applies the bisection method on a half-open interval \([l, r)\) and returns the
-- left boundary point.
--
-- @
-- N N N N N Y Y Y Y Y      Y: p(i) returns `true`
-- --------* ----------> x  N: not Y
--         L                L: the left boundary point returned
-- @
--
-- __Known bug__: user function \(p\) takes __closed intervals__ \([l, r]\).
--
-- ==== __Example__
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let xs = VU.fromList [10, 10, 20, 20, 30, 30]
-- >>> let n = VU.length xs
-- >>> minLeft 0 n ((>= 20) . (xs VU.!))
-- 2
--
-- >>> minLeft 0 n ((>= 0) . (xs VU.!))
-- 0
--
-- >>> minLeft 0 n ((>= 100) . (xs VU.!))
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
  -- | \(p\)
  (Int -> Bool) ->
  -- | Minimum \(l' (l' \ge l)\) where \(p(i)\) holds for \(i \in [l', r)\)
  Int
minLeft l r p = runIdentity $ minLeftM l r (pure . p)

-- | \(O(\log n)\) Monadic variant of `minLeft`.
--
-- __Known bug__: user function \(p\) takes __closed intervals__ \([l, r]\).
--
-- @since 1.3.0.0
{-# INLINE minLeftM #-}
minLeftM :: (HasCallStack, Monad m) => Int -> Int -> (Int -> m Bool) -> m Int
minLeftM l r p = (+ 1) <$> bisectImpl r (l - 1) p
  where
    !_ = ACIA.checkInterval "AtCoder.Extra.Bisect.minLeftM" l r

-- | Takes an open interval (l, r) or (r, l).
{-# INLINE bisectImpl #-}
bisectImpl :: (HasCallStack, Monad m) => Int -> Int -> (Int -> m Bool) -> m Int
bisectImpl l0 r0 p = inner l0 r0
  where
    inner l r
      | abs (r - l) <= 1 = pure r
      | otherwise =
          p mid >>= \case
            True -> inner mid r
            False -> inner l mid
      where
        mid = (l + r) `div` 2
