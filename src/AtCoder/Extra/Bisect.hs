{-# LANGUAGE LambdaCase #-}

-- | Bisection methods and binary search functions. They partition a half-open interval \([l, r)\)
-- into two and return either the left or the right point of the boundary.
--
-- @
-- Y Y Y Y Y N N N N N      Y: user predicate holds
-- --------* *---------> X  N: user predicate does not hold
--         L R              L, R: left, right point of the boundary
-- @
--
-- ==== __Example__
-- Perform index compression:
--
-- >>> import AtCoder.Extra.Bisect
-- >>> import Data.Maybe (fromJust)
-- >>> import Data.Vector.Algorithms.Intro qualified as VAI
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let xs = VU.fromList ([0, 20, 10, 40, 30] :: [Int])
-- >>> let dict = VU.uniq $ VU.modify VAI.sort xs
-- >>> VU.map (fromJust . lowerBound dict) xs
-- [0,2,1,4,3]
--
-- @since 1.1.0.0
module AtCoder.Extra.Bisect
  ( -- * C++-like binary search
    lowerBound,
    lowerBoundIn,
    upperBound,
    upperBoundIn,

    -- * Generic bisection method
    bisectL,
    bisectLM,
    bisectR,
    bisectRM,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import Data.Functor ((<&>))
import Data.Functor.Identity
import Data.Vector.Generic qualified as VG
import GHC.Stack (HasCallStack)

-- | \(O(\log n)\) Bisection method implementation. Works on a half-open interfal \([l, r)\) .
--
-- @since 1.1.0.0
{-# INLINE bisectLImpl #-}
bisectLImpl :: (HasCallStack, Monad m) => (Int -> m Bool) -> Int -> Int -> m Int
bisectLImpl p l0 = inner (l0 - 1)
  where
    inner l r
      | l + 1 == r = pure l
      | otherwise =
          p mid >>= \case
            True -> inner mid r
            False -> inner l mid
      where
        mid = (l + r) `div` 2

-- | \(O(\log n)\) Bisection method implementation. Works on a half-open interfal \([l, r)\) .
--
-- @since 1.1.0.0
{-# INLINE bisectRImpl #-}
bisectRImpl :: (HasCallStack, Monad m) => (Int -> m Bool) -> Int -> Int -> m Int
bisectRImpl p l = ((+ 1) <$>) . bisectLImpl p l

-- | \(O(\log n)\) Returns the index of the first element \(x\) in the vector such that
-- \(x \ge x_0\), or `Nothing` if no such element exists.
--
-- @
-- Y Y Y Y Y N N N N N      Y: (< x0)
-- --------- *---------> X  N: (>= x0)
--           R              R: returning point
-- @
--
-- ==== __Example__
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let xs = VU.fromList [1, 1, 2, 2, 4, 4]
-- >>> lowerBound xs 1
-- Just 0
--
-- >>> lowerBound xs 2
-- Just 2
--
-- >>> lowerBound xs 3
-- Just 4
--
-- >>> lowerBound xs 4
-- Just 4
--
-- Out of range values:
--
-- >>> lowerBound xs 0
-- Just 0
--
-- >>> lowerBound xs 5
-- Nothing
--
-- @since 1.1.0.0
{-# INLINE lowerBound #-}
lowerBound :: (HasCallStack, VG.Vector v a, Ord a) => v a -> a -> Maybe Int
lowerBound vec = lowerBoundIn 0 (VG.length vec) vec

-- | \(O(\log n)\) Computes the `lowerBound` for a slice of a vector within the interval \([l, r)\).
--
-- - The user predicate evaluates indices in \([l, r)\).
-- - The interval \([l, r)\) is silently clamped to ensure it remains within the bounds \([0, n)\).
--
-- ==== __Example__
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let xs = VU.fromList [10, 10, 20, 20, 40, 40]
-- >>> --                            *---*---*
-- >>> lowerBoundIn 2 5 xs 10
-- Just 2
--
-- >>> lowerBoundIn 2 5 xs 20
-- Just 2
--
-- >>> lowerBoundIn 2 5 xs 30
-- Just 4
--
-- >>> lowerBoundIn 2 5 xs 40
-- Just 4
--
-- >>> lowerBoundIn 2 5 xs 50
-- Nothing
--
-- @since 1.1.0.0
{-# INLINE lowerBoundIn #-}
lowerBoundIn :: (VG.Vector v a, Ord a) => Int -> Int -> v a -> a -> Maybe Int
lowerBoundIn l_ r_ vec target
  | ACIA.testInterval l r (VG.length vec) = bisectR l r $ \i -> VG.unsafeIndex vec i < target
  | otherwise = Nothing
  where
    -- clamp
    l = max 0 l_
    r = min (VG.length vec) r_

-- | \(O(\log n)\) Returns the index of the first element \(x\) in the vector such that
-- \(x \gt x_0\), or `Nothing` if no such element exists.
--
-- @
-- Y Y Y Y Y N N N N N      Y: (<= x0)
-- --------- *---------> X  N: (> x0)
--           R              R: returning point
-- @
--
-- ==== __Example__
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let xs = VU.fromList [10, 10, 20, 20, 40, 40]
-- >>> upperBound xs 10
-- Just 2
--
-- >>> upperBound xs 20
-- Just 4
--
-- >>> upperBound xs 30
-- Just 4
--
-- >>> upperBound xs 40
-- Nothing
--
-- Out of range values:
--
-- >>> upperBound xs 0
-- Just 0
--
-- >>> upperBound xs 50
-- Nothing
--
-- @since 1.1.0.0
{-# INLINE upperBound #-}
upperBound :: (HasCallStack, VG.Vector v a, Ord a) => v a -> a -> Maybe Int
upperBound vec = upperBoundIn 0 (VG.length vec) vec

-- | \(O(\log n)\) Computes the `upperBound` for a slice of a vector within the interval \([l, r)\).
--
-- - The user predicate evaluates indices in \([l, r)\).
-- - The interval \([l, r)\) is silently clamped to ensure it remains within the bounds \([0, n)\).
--
-- ==== __Example__
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let xs = VU.fromList [10, 10, 20, 20, 40, 40]
-- >>> --                            *---*---*
-- >>> upperBoundIn 2 5 xs 0
-- Just 2
--
-- >>> upperBoundIn 2 5 xs 10
-- Just 2
--
-- >>> upperBoundIn 2 5 xs 20
-- Just 4
--
-- >>> upperBoundIn 2 5 xs 30
-- Just 4
--
-- >>> upperBoundIn 2 5 xs 40
-- Nothing
--
-- >>> upperBoundIn 2 5 xs 50
-- Nothing
--
-- @since 1.1.0.0
{-# INLINE upperBoundIn #-}
upperBoundIn :: (VG.Vector v a, Ord a) => Int -> Int -> v a -> a -> Maybe Int
upperBoundIn l_ r_ vec target
  | ACIA.testInterval l r (VG.length vec) = bisectR l r $ \i -> VG.unsafeIndex vec i <= target
  | otherwise = Nothing
  where
    -- clamp
    l = max 0 l_
    r = min (VG.length vec) r_

-- | \(O(\log n)\) Applies the bisection method on a half-open interval \([l, r)\) and returns the
-- left boundary point, or `Nothing` if no such point exists.
--
-- @
-- Y Y Y Y Y N N N N N      Y: user predicate holds
-- --------* ----------> X  N: user predicate does not hold
--         L                L: the left boundary point returned
-- @
--
-- ==== __Example__
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let xs = VU.fromList [10, 10, 20, 20, 30, 30]
-- >>> let n = VU.length xs
-- >>> bisectL 0 n ((<= 20) . (xs VU.!))
-- Just 3
--
-- >>> bisectL 0 n ((<= 0) . (xs VU.!))
-- Nothing
--
-- >>> bisectL 0 n ((<= 100) . (xs VU.!))
-- Just 5
--
-- >>> bisectL 0 3 ((<= 20) . (xs VU.!))
-- Just 2
--
-- @since 1.1.0.0
{-# INLINE bisectL #-}
bisectL :: (HasCallStack) => Int -> Int -> (Int -> Bool) -> Maybe Int
bisectL l r p = runIdentity $ bisectLM l r (pure . p)

-- | \(O(\log n)\) Monadic variant of `bisectL`.
--
-- @since 1.1.0.0
{-# INLINE bisectLM #-}
bisectLM :: (HasCallStack, Monad m) => Int -> Int -> (Int -> m Bool) -> m (Maybe Int)
bisectLM l r p
  | l >= r = pure Nothing
  | otherwise =
      bisectLImpl p l r <&> \case
        i | i == (l - 1) -> Nothing
        i -> Just i

-- | \(O(\log n)\) Applies the bisection method on a half-open interval \([l, r)\) and returns the
-- right boundary point, or `Nothing` if no such point exists.
--
--
-- @
-- Y Y Y Y Y N N N N N      Y: user predicate holds
-- --------- *---------> X  N: user predicate does not hold
--           R              R: the right boundary point returned
-- @
--
-- ==== __Example__
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let xs = VU.fromList [10, 10, 20, 20, 30, 30]
-- >>> let n = VU.length xs
-- >>> bisectR 0 n ((<= 20) . (xs VU.!))
-- Just 4
--
-- >>> bisectR 0 n ((<= 0) . (xs VU.!))
-- Just 0
--
-- >>> bisectR 0 n ((<= 100) . (xs VU.!))
-- Nothing
--
-- >>> bisectR 0 4 ((<= 20) . (xs VU.!))
-- Nothing
--
-- @since 1.1.0.0
{-# INLINE bisectR #-}
bisectR :: (HasCallStack) => Int -> Int -> (Int -> Bool) -> Maybe Int
bisectR l r p = runIdentity $ bisectRM l r (pure . p)

-- | \(O(\log n)\) Monadic variant of `bisectR`.
--
-- @since 1.1.0.0
{-# INLINE bisectRM #-}
bisectRM :: (HasCallStack, Monad m) => Int -> Int -> (Int -> m Bool) -> m (Maybe Int)
bisectRM l r p
  | l >= r = pure Nothing
  | otherwise =
      bisectRImpl p l r <&> \case
        i | i == r -> Nothing
        i -> Just i
