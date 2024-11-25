{-# LANGUAGE RecordWildCards #-}

-- | It is the data structure for [monoids](https://en.wikipedia.org/wiki/Monoid)
-- \((S, \cdot: S \times S \to S, e \in S)\), i.e., the algebraic structure that satisfies the
-- following properties.
--
-- - associativity: \((a \cdot b) \cdot c\) = \(a \cdot (b \cdot c)\) for all \(a, b, c \in S\)
-- - existence of the identity element: \(a \cdot e\) = \(e \cdot a\) = \(a\) for all \(a \in S\)
--
-- Given an array \(S\) of length \(N\), it processes the following queries in \(O(\log N)\) time
-- (see [Appendix](./appendix.html) for further details).
--
-- - Updating an element
-- - Calculating the product of the elements of an interval
--
-- For simplicity, in this document, we assume that the oracles @op@ and @e@ work in constant time.
-- If these oracles work in \(O(T)\) time, each time complexity appear in this document is
-- multipled by \(O(T)\).
--
-- = Storing boxed types
-- If you really need to store boxed type to `SegTree`, wrap them in a newtype that implements
-- @Unbox@ typeclass via a boxed vector, such as [@DoNotUnboxStrict a@](https://hackage.haskell.org/package/vector-0.13.2.0/docs/Data-Vector-Unboxed.html#t:DoNotUnboxStrict).
--
-- = Example
-- >>> import AtCoder.SegTree qualified as ST
-- >>> import Data.Monoid (Sum(..))
-- >>> -- Create an array [0, 3, 0, 2]
-- >>> seg <- ST.new @_ @(Sum Int) 4
-- >>> ST.write seg 1 $ Sum 3
-- >>> ST.write seg 3 $ Sum 2
-- >>> -- Use read methods
-- >>> ST.read seg 1
-- Sum {getSum = 3}
-- >>> ST.prod seg 0 3
-- Sum {getSum = 3}
-- >>> ST.allProd seg
-- Sum {getSum = 5}
-- >>> ST.maxRight seg 0 (< (Sum 5)) -- sum [0, 3) = 3 < 5
-- 3
-- >>> ST.minLeft seg 4 (< (Sum 5)) -- sum [2, 4) = 2 < 5
-- 2
--
-- = Major changes from the original @ac-library@
-- - @get@ and @set@ are renamed to `read` and `write`.
-- - The implementation is `Monoid`-based.
module AtCoder.SegTree
  ( SegTree (nSt, sizeSt, logSt),
    new,
    build,
    write,
    read,
    prod,
    allProd,
    maxRight,
    minLeft,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Bit qualified as ACIBIT
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bits (countTrailingZeros, testBit, (.&.), (.>>.))
import Data.Foldable (for_)
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)
import Prelude hiding (read)

data SegTree s a = SegTree
  { -- | Valid length.
    nSt :: {-# UNPACK #-} !Int,
    -- | \(\lceil \log_2 \mathrm{nSt} \rceil\)
    sizeSt :: {-# UNPACK #-} !Int,
    -- | \(\log_2 \mathrm{sizeSt}\).
    logSt :: {-# UNPACK #-} !Int,
    -- | Data storage of length @2 * sizeSt@.
    dSt :: !(VUM.MVector s a)
  }

-- | Creates an array @a@ of length @n@. All the elements are initialized to `mempty`.
--
-- = Constraints
-- - \(0 \leq n\)
--
-- = Complexity
-- - \(O(n)\)
new :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Int -> m (SegTree (PrimState m) a)
new nSt
  | nSt >= 0 = build $ VU.replicate nSt mempty
  | otherwise = error $ "new: given negative size (`" ++ show nSt ++ "`)"

-- | Creates an array with initial values.
--
-- = Complexity
-- - \(O(n)\)
build :: (PrimMonad m, Monoid a, VU.Unbox a) => VU.Vector a -> m (SegTree (PrimState m) a)
build vs = do
  let nSt = VU.length vs
  let sizeSt = ACIBIT.bitCeil nSt
  let logSt = countTrailingZeros sizeSt
  dSt <- VUM.replicate (2 * sizeSt) mempty
  VU.iforM_ vs $ \i v -> do
    VGM.write dSt (sizeSt + i) v
  let segtree = SegTree {..}
  for_ [sizeSt - 1, sizeSt - 2 .. 1] $ \i -> do
    update segtree i
  pure segtree

-- | Writes \(p\)-th value of the array to \(x\).
--
-- = Constraints
-- - \(0 \leq p \lt n\)
--
-- = Complexity
-- - \(O(1)\)
write :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => SegTree (PrimState m) a -> Int -> a -> m ()
write self@SegTree {..} p x = do
  let !_ = ACIA.checkIndex "AtCoder.SegTree.write" p nSt
  VGM.write dSt (p + sizeSt) x
  for_ [1 .. logSt] $ \i -> do
    update self ((p + sizeSt) .>>. i)

-- | Returns \(p\)-th value of the array.
--
-- = Constraints
-- - \(0 \leq p \lt n\)
--
-- = Complexity
-- - \(O(\log n)\)
read :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => SegTree (PrimState m) a -> Int -> m a
read SegTree {..} p = do
  let !_ = ACIA.checkIndex "AtCoder.SegTree.read" p nSt
  VGM.read dSt $ p + sizeSt

-- | Returns @a[l] <> ... <> a[r - 1]@, assuming the properties of the monoid. It returns `mempty`
-- if \(l = r\).
--
-- = Constraints
-- - \(0 \leq l \leq r \leq n\)
--
-- = Complexity
-- - \(O(\log n)\)
prod :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => SegTree (PrimState m) a -> Int -> Int -> m a
prod SegTree {..} l0 r0 = inner (l0 + sizeSt) (r0 + sizeSt - 1) mempty mempty
  where
    !_ = ACIA.checkInterval "AtCoder.SegTree.prod" l0 r0 nSt
    -- NOTE: we're using inclusive range [l, r] for simplicity
    inner l r !smL !smR
      | l > r = pure $! smL <> smR
      | otherwise = do
          !smL' <-
            if testBit l 0
              then (smL <>) <$> VGM.read dSt l
              else pure smL
          !smR' <-
            if not $ testBit r 0
              then (<> smR) <$> VGM.read dSt r
              else pure smR
          inner ((l + 1) .>>. 1) ((r - 1) .>>. 1) smL' smR'

-- | Returns @a[0] <> ... <> a[n - 1]@, assuming the properties of the monoid. It returns `mempty`
-- if \(n = 0\).
--
-- = Complexity
-- - \(O(1)\)
allProd :: (PrimMonad m, Monoid a, VU.Unbox a) => SegTree (PrimState m) a -> m a
allProd SegTree {..} = VGM.read dSt 1

-- | Applies binary search on the segment tree. It returns an index @r@ that satisfies both of the
-- following.
--
-- - @r == l@ or @a[l] <> a[l + 1] <> ... <> a[r - 1] == True@
-- - @r == n@ or @a[l] <> a[l + 1] <> ... <> a[r]) == False@
--
-- If @f@ is monotone, this is the maximum @r@ that satisfies
-- @f(a[l] <> a[l + 1] <> ... <> a[r - 1]) == True@.
--
-- = Constraints
-- - if @f@ is called with the same argument, it returns the same value, i.e., @f@ has no side effect.
-- - @f mempty == True@
-- - \(0 \leq l \leq n\)
--
-- = Complexity
-- - \(O(\log n)\)
maxRight :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => SegTree (PrimState m) a -> Int -> (a -> Bool) -> m Int
maxRight SegTree {..} l0 f
  | l0 == nSt = pure nSt
  | otherwise = inner (l0 + sizeSt) mempty
  where
    -- NOTE: Not ordinary bounds check!
    !_ = ACIA.runtimeAssert (0 <= l0 && l0 <= nSt) $ "AtCoder.SegTree.maxRight: given invalid `left` index `" ++ show l0 ++ "` over length `" ++ show nSt ++ "`"
    !_ = ACIA.runtimeAssert (f mempty) "AtCoder.SegTree.maxRight: `f mempty` returned `False`"
    inner l !sm = do
      let l' = chooseBit l
      !sm' <- (sm <>) <$> VGM.read dSt l'
      if not $ f sm'
        then do
          inner2 l' sm
        else do
          let l'' = l' + 1
          if (l'' .&. (-l'')) /= l''
            then inner l'' sm'
            else pure nSt
    chooseBit :: Int -> Int
    chooseBit l
      | even l = chooseBit $ l .>>. 1
      | otherwise = l
    inner2 l !sm
      | l < sizeSt = do
          let l' = 2 * l
          !sm' <- (sm <>) <$> VGM.read dSt l'
          if f sm'
            then inner2 (l' + 1) sm'
            else inner2 l' sm
      | otherwise = pure $ l - sizeSt

-- | It applies binary search on the segment tree. It returns an index @l@ that satisfies both of
-- the following.
--
-- - @l == r@ or @f(a[l] <> a[l + 1] <> ... <> a[r - 1]) == True@
-- - @l == 0@ or @f(a[l - 1] <> a[l] <> ... <> a[r - 1]) == False@
--
-- = Constraints
--
-- - if @f@ is called with the same argument, it returns the same value, i.e., @f@ has no side
--   effect.
-- - @f mempty == True@
-- - \(0 \leq r \leq n\)
--
-- = Complexity
-- - \(O(\log n)\)
minLeft :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => SegTree (PrimState m) a -> Int -> (a -> Bool) -> m Int
minLeft SegTree {..} r0 f
  | r0 == 0 = pure 0
  | otherwise = inner (r0 + sizeSt) mempty
  where
    -- NOTE: Not ordinary bounds check!
    !_ = ACIA.runtimeAssert (0 <= r0 && r0 <= nSt) $ "AtCoder.SegTree.minLeft: given invalid `right` index `" ++ show r0 ++ "` over length `" ++ show nSt ++ "`"
    !_ = ACIA.runtimeAssert (f mempty) "AtCoder.SegTree.minLeft: `f empty` returned `False`"
    inner r !sm = do
      let r' = chooseBit $ r - 1
      !sm' <- (<> sm) <$> VGM.read dSt r'
      if not $ f sm'
        then do
          inner2 r' sm
        else do
          if (r' .&. (-r')) /= r'
            then inner r' sm'
            else pure 0
    chooseBit r
      | r > 1 && odd r = chooseBit $ r .>>. 1
      | otherwise = r
    inner2 r sm
      | r < sizeSt = do
          let r' = 2 * r + 1
          !sm' <- (<> sm) <$> VGM.read dSt r'
          if f sm'
            then inner2 (r' - 1) sm'
            else inner2 r' sm
      | otherwise = pure $ r + 1 - sizeSt

-- | \(O(1)\)
update :: (PrimMonad m, Monoid a, VU.Unbox a) => SegTree (PrimState m) a -> Int -> m ()
update SegTree {..} k = do
  opL <- VGM.read dSt $ 2 * k
  opR <- VGM.read dSt $ 2 * k + 1
  VGM.write dSt k $! opL <> opR
