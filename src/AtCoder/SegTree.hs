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
-- ==== __Example__
-- Create a `SegTree` of @'Sum' Int@:
--
-- >>> import AtCoder.SegTree qualified as ST
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> import Data.Monoid (Sum(..))
-- >>> seg <- ST.new @_ @(Sum Int) 4
--
-- Modify the vertex values:
--
-- >>> ST.write seg 1 $ Sum 1
-- >>> ST.modify seg (+ Sum 2) 2
-- >>> ST.write seg 3 $ Sum 3 -- [0, 1, 2, 3]
-- >>> ST.read seg 1
-- Sum {getSum = 1}
--
-- Get product of the monoids:
--
-- >>> ST.prod seg 0 3
-- Sum {getSum = 3}
--
-- >>> ST.allProd seg
-- Sum {getSum = 6}
--
-- Binary searches:
--
-- >>> ST.maxRight seg 0 (< (Sum 5)) -- sum [0, 3) = 2 < 5
-- 3
--
-- >>> ST.minLeft seg 4 (< (Sum 5)) -- sum [3, 4) = 3 < 5
-- 3
--
-- Inspect all the values in \(O(n)\) with `freeze` or in \(O(1)\) with `unsafeFreeze`:
--
-- >>> VU.map getSum <$> ST.freeze seg
-- [0,1,2,3]
--
-- ==== Tips
--
-- - `prod` returns \(a_l \cdot a_{l + 1} \cdot .. \cdot a_{r - 1}\). If you need \(a_{r - 1} \cdot a_{r - 2} \cdot .. \cdot a_{l}\),
-- wrap your monoid in `Data.Monoid.Dual`.
-- - If you ever need to store boxed types to `LazySegTree`, wrap it in @Data.Vector.Unboxed.DoNotUnboxStrict@
-- or the like.
--
-- ==== Major changes from the original @ac-library@
-- - The implementation is `Monoid` based, not function objects.
-- - @get@ and @set@ are renamed to `read` and `write`.
-- - `modify`, `modifyM`, `exchange`, `freeze` and `unsafeFreeze` are added.
--
-- @since 1.0.0.0
module AtCoder.SegTree
  ( -- * Segment tree
    SegTree (nSt, sizeSt, logSt),

    -- * Constructors
    new,
    build,

    -- * Accessing elements
    write,
    modify,
    modifyM,
    exchange,
    read,

    -- * Products
    prod,
    prodMaybe,
    allProd,

    -- * Binary searches

    -- ** Left binary searches
    minLeft,
    minLeftM,

    -- ** Right binary searches
    maxRight,
    maxRightM,

    -- * Conversions
    freeze,
    unsafeFreeze,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Bit qualified as ACIBIT
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Control.Monad.ST (ST)
import Data.Bits (countTrailingZeros, testBit, (.&.), (.>>.))
import Data.Foldable (for_)
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)
import Prelude hiding (read)

-- | Segment tree.
--
-- @since 1.0.0.0
data SegTree s a = SegTree
  { -- | THe number of vertices.
    --
    -- @since 1.0.0.0
    nSt :: {-# UNPACK #-} !Int,
    -- | \(\lceil \log_2 \mathrm{nSt} \rceil\).
    --
    -- @since 1.0.0.0
    sizeSt :: {-# UNPACK #-} !Int,
    -- | \(\log_2 \mathrm{sizeSt}\).
    --
    -- @since 1.0.0.0
    logSt :: {-# UNPACK #-} !Int,
    -- | Data storage of length @2 * sizeSt@.
    dSt :: !(VUM.MVector s a)
  }

-- | Creates an array \(a\) of length \(n\). All the elements are initialized to `mempty`.
--
-- ==== Constraints
-- - \(0 \leq n\)
--
-- ==== Complexity
-- - \(O(n)\)
--
-- @since 1.0.0.0
{-# INLINE new #-}
new :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Int -> m (SegTree (PrimState m) a)
new n = stToPrim $ newST n

-- | Creates an array with initial values.
--
-- ==== Complexity
-- - \(O(n)\)
--
-- @since 1.0.0.0
{-# INLINE build #-}
build :: (PrimMonad m, Monoid a, VU.Unbox a) => VU.Vector a -> m (SegTree (PrimState m) a)
build vs = stToPrim $ buildST vs

-- | Writes \(p\)-th value of the array to \(x\).
--
-- ==== Constraints
-- - \(0 \leq p \lt n\)
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- @since 1.0.0.0
{-# INLINE write #-}
write :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => SegTree (PrimState m) a -> Int -> a -> m ()
write self p x = stToPrim $ writeST self p x

-- | (Extra API) Modifies \(p\)-th value with a function \(f\).
--
-- ==== Constraints
-- - \(0 \leq p \lt n\)
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- @since 1.0.0.0
{-# INLINE modify #-}
modify :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => SegTree (PrimState m) a -> (a -> a) -> Int -> m ()
modify self f p = stToPrim $ modifyST self f p

-- | (Extra API) Modifies \(p\)-th value with a monadic function \(f\).
--
-- ==== Constraints
-- - \(0 \leq p \lt n\)
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- @since 1.0.0.0
{-# INLINE modifyM #-}
modifyM :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => SegTree (PrimState m) a -> (a -> m a) -> Int -> m ()
modifyM self@SegTree {..} f p = do
  let !_ = ACIA.checkIndex "AtCoder.SegTree.modifyM" p nSt
  VGM.modifyM dSt f (p + sizeSt)
  stToPrim $ for_ [1 .. logSt] $ \i -> do
    updateST self ((p + sizeSt) .>>. i)

-- | (Extra API) Writes \(p\)-th value of the array to \(x\) and returns the old value.
--
-- ==== Constraints
-- - \(0 \leq p \lt n\)
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- @since 1.1.0.0
{-# INLINE exchange #-}
exchange :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => SegTree (PrimState m) a -> Int -> a -> m a
exchange self p x = stToPrim $ exchangeST self p x

-- | Returns \(p\)-th value of the array.
--
-- ==== Constraints
-- - \(0 \leq p \lt n\)
--
-- ==== Complexity
-- - \(O(1)\)
--
-- @since 1.0.0.0
{-# INLINE read #-}
read :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => SegTree (PrimState m) a -> Int -> m a
read SegTree {..} p = do
  let !_ = ACIA.checkIndex "AtCoder.SegTree.read" p nSt
  VGM.read dSt $ p + sizeSt

-- | Returns \(a[l] \cdot ... \cdot a[r - 1]\), assuming the properties of the monoid. It
-- returns `mempty` if \(l = r\).
--
-- ==== Constraints
-- - \(0 \leq l \leq r \leq n\)
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- @since 1.0.0.0
{-# INLINE prod #-}
prod :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => SegTree (PrimState m) a -> Int -> Int -> m a
prod self@SegTree {nSt} l0 r0
  | ACIA.testInterval l0 r0 nSt = stToPrim $ unsafeProdST self l0 r0
  | otherwise = ACIA.errorInterval "AtCoder.SegTree.prod" l0 r0 nSt

-- | Total variant of `prod`. Returns \(a[l] \cdot ... \cdot a[r - 1]\), assuming the properties of
-- the monoid. It returns `Just` `mempty` if \(l = r\). Returns `Nothing` if the interval is
-- invalid.
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- @since 1.0.0.0
{-# INLINE prodMaybe #-}
prodMaybe :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => SegTree (PrimState m) a -> Int -> Int -> m (Maybe a)
prodMaybe self@SegTree {nSt} l0 r0
  | ACIA.testInterval l0 r0 nSt = stToPrim $ Just <$> unsafeProdST self l0 r0
  -- l0 == r0 = pure (Just mempty)
  | otherwise = pure Nothing

-- | Returns @a[0] <> ... <> a[n - 1]@, assuming the properties of the monoid. It returns `mempty`
-- if \(n = 0\).
--
-- ==== Complexity
-- - \(O(1)\)
--
-- @since 1.0.0.0
{-# INLINE allProd #-}
allProd :: (PrimMonad m, Monoid a, VU.Unbox a) => SegTree (PrimState m) a -> m a
allProd SegTree {..} = VGM.read dSt 1

-- | Applies a binary search on the segment tree. It returns an index \(l\) that satisfies both of
-- the following.
--
-- - \(l = r\) or \(f(a[l] \cdot a[l + 1] \cdot ... \cdot a[r - 1])\) returns `True`.
-- - \(l = 0\) or \(f(a[l - 1] \cdot a[l] \cdot ... \cdot a[r - 1])\) returns `False`.
--
-- If \(f\) is monotone, this is the minimum \(l\) that satisfies
-- \(f(a[l] \cdot a[l + 1] \cdot ... \cdot a[r - 1])\).
--
-- ==== Constraints
--
-- - if \(f\) is called with the same argument, it returns the same value, i.e., \(f\) has no side
--   effect.
-- - @f mempty == True@.
-- - \(0 \leq r \leq n\)
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- @since 1.0.0.0
{-# INLINE minLeft #-}
minLeft ::
  (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) =>
  -- | The segment tree
  SegTree (PrimState m) a ->
  -- | \(r\)
  Int ->
  -- | \(p\): user prediate
  (a -> Bool) ->
  -- | \(l\): \(p\) holds for \([l, r)\)
  m Int
minLeft seg r0 f = minLeftM seg r0 (pure . f)

-- | Monadic variant of `minLeft`.
--
-- ==== Constraints
--
-- - if \(f\) is called with the same argument, it returns the same value, i.e., \(f\) has no side
--   effect.
-- - @f mempty == True@.
-- - \(0 \leq r \leq n\)
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- @since 1.0.0.0
{-# INLINEABLE minLeftM #-}
minLeftM ::
  (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) =>
  -- | The segment tree
  SegTree (PrimState m) a ->
  -- | \(r\)
  Int ->
  -- | \(p\): user prediate
  (a -> m Bool) ->
  -- | \(l\): \(p\) holds for \([l, r)\)
  m Int
minLeftM SegTree {..} r0 f = do
  b <- f mempty
  let !_ = ACIA.runtimeAssert b "AtCoder.SegTree.minLeftM: `f empty` returned `False`"
  if r0 == 0
    then pure 0
    else inner (r0 + sizeSt) mempty
  where
    -- NOTE: Not ordinary bounds check!
    !_ = ACIA.runtimeAssert (0 <= r0 && r0 <= nSt) $ "AtCoder.SegTree.minLeftM: given invalid `right` index `" ++ show r0 ++ "` over length `" ++ show nSt ++ "`"
    inner r !sm = do
      let r' = chooseBit $ r - 1
      !sm' <- stToPrim $ (<> sm) <$> VGM.read dSt r'
      b <- f sm'
      if not b
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
          !sm' <- stToPrim $ (<> sm) <$> VGM.read dSt r'
          b <- f sm'
          if b
            then inner2 (r' - 1) sm'
            else inner2 r' sm
      | otherwise = pure $ r + 1 - sizeSt

-- | Applies a binary search on the segment tree. It returns an index \(r\) that satisfies both of the
-- following.
--
-- - \(r = l\) or \(f(a[l] \cdot a[l + 1] \cdot ... \cdot a[r - 1])\) returns `True`.
-- - \(r = n\) or \(f(a[l] \cdot a[l + 1] \cdot ... \cdot a[r]))\) returns `False`.
--
-- If \(f\) is monotone, this is the maximum \(r\) that satisfies
-- \(f(a[l] \cdot a[l + 1] \cdot ... \cdot a[r - 1])\).
--
-- ==== Constraints
-- - if \(f\) is called with the same argument, it returns the same value, i.e., \(f\) has no side effect.
-- - @f mempty == True@.
-- - \(0 \leq l \leq n\)
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- @since 1.0.0.0
{-# INLINE maxRight #-}
maxRight ::
  (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) =>
  -- | The segment tree
  SegTree (PrimState m) a ->
  -- | \(l\)
  Int ->
  -- | \(p\): user prediate
  (a -> Bool) ->
  -- | \(r\): \(p\) holds for \([l, r)\)
  m Int
maxRight seg l0 f = maxRightM seg l0 (pure . f)

-- | Moandic variant of `maxRight`.
--
-- ==== Constraints
-- - if \(f\) is called with the same argument, it returns the same value, i.e., \(f\) has no side effect.
-- - @f mempty == True@.
-- - \(0 \leq l \leq n\)
--
-- ==== Complexity
-- - \(O(\log n)\)
--
-- @since 1.0.0.0
{-# INLINEABLE maxRightM #-}
maxRightM ::
  (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) =>
  -- | The segment tree
  SegTree (PrimState m) a ->
  -- | \(l\)
  Int ->
  -- | \(p\): user prediate
  (a -> m Bool) ->
  -- | \(r\): \(p\) holds for \([l, r)\)
  m Int
maxRightM SegTree {..} l0 f = do
  b <- f mempty
  let !_ = ACIA.runtimeAssert b "AtCoder.SegTree.maxRightM: `f mempty` must return `True`"
  if l0 == nSt
    then pure nSt
    else inner (l0 + sizeSt) mempty
  where
    -- NOTE: Not ordinary bounds check!
    !_ = ACIA.runtimeAssert (0 <= l0 && l0 <= nSt) $ "AtCoder.SegTree.maxRightM: given invalid `left` index `" ++ show l0 ++ "` over length `" ++ show nSt ++ "`"
    inner l !sm = do
      let l' = chooseBit l
      !sm' <- stToPrim $ (sm <>) <$> VGM.read dSt l'
      b <- f sm'
      if not b
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
          !sm' <- stToPrim $ (sm <>) <$> VGM.read dSt l'
          b <- f sm'
          if b
            then inner2 (l' + 1) sm'
            else inner2 l' sm
      | otherwise = pure $ l - sizeSt

-- | \(O(n)\) Yields an immutable copy of the mutable vector.
--
-- @since 1.0.0.0
{-# INLINE freeze #-}
freeze :: (PrimMonad m, VU.Unbox a) => SegTree (PrimState m) a -> m (VU.Vector a)
freeze SegTree {..} = do
  VU.freeze . VUM.take nSt $ VUM.drop sizeSt dSt

-- | \(O(1)\) Unsafely converts a mutable vector to an immutable one without copying. The mutable
-- vector may not be used after this operation.
--
-- @since 1.0.0.0
{-# INLINE unsafeFreeze #-}
unsafeFreeze :: (PrimMonad m, VU.Unbox a) => SegTree (PrimState m) a -> m (VU.Vector a)
unsafeFreeze SegTree {..} = do
  VU.unsafeFreeze . VUM.take nSt $ VUM.drop sizeSt dSt

-- -------------------------------------------------------------------------------------------------
-- Internal
-- -------------------------------------------------------------------------------------------------

{-# INLINEABLE newST #-}
newST :: (HasCallStack, Monoid a, VU.Unbox a) => Int -> ST s (SegTree s a)
newST nSt
  | nSt >= 0 = build $ VU.replicate nSt mempty
  | otherwise = error $ "AtCoder.SegTree.newST: given negative size (`" ++ show nSt ++ "`)"

{-# INLINEABLE buildST #-}
buildST :: (Monoid a, VU.Unbox a) => VU.Vector a -> ST s (SegTree s a)
buildST vs = do
  let nSt = VU.length vs
  let sizeSt = ACIBIT.bitCeil nSt
  let logSt = countTrailingZeros sizeSt
  dSt <- VUM.replicate (2 * sizeSt) mempty
  VU.iforM_ vs $ \i v -> do
    VGM.write dSt (sizeSt + i) v
  let segtree = SegTree {..}
  for_ [sizeSt - 1, sizeSt - 2 .. 1] $ \i -> do
    updateST segtree i
  pure segtree

{-# INLINEABLE writeST #-}
writeST :: (HasCallStack, Monoid a, VU.Unbox a) => SegTree s a -> Int -> a -> ST s ()
writeST self@SegTree {..} p x = do
  let !_ = ACIA.checkIndex "AtCoder.SegTree.writeST" p nSt
  VGM.write dSt (p + sizeSt) x
  for_ [1 .. logSt] $ \i -> do
    updateST self ((p + sizeSt) .>>. i)

{-# INLINEABLE modifyST #-}
modifyST :: (HasCallStack, Monoid a, VU.Unbox a) => SegTree s a -> (a -> a) -> Int -> ST s ()
modifyST self@SegTree {..} f p = do
  let !_ = ACIA.checkIndex "AtCoder.SegTree.modifyST" p nSt
  VGM.modify dSt f (p + sizeSt)
  for_ [1 .. logSt] $ \i -> do
    updateST self ((p + sizeSt) .>>. i)

{-# INLINEABLE exchangeST #-}
exchangeST :: (HasCallStack, Monoid a, VU.Unbox a) => SegTree s a -> Int -> a -> ST s a
exchangeST self@SegTree {..} p x = do
  let !_ = ACIA.checkIndex "AtCoder.SegTree.exchangeST" p nSt
  ret <- VGM.exchange dSt (p + sizeSt) x
  VGM.write dSt (p + sizeSt) x
  for_ [1 .. logSt] $ \i -> do
    updateST self ((p + sizeSt) .>>. i)
  pure ret

{-# INLINEABLE unsafeProdST #-}
unsafeProdST :: (Monoid a, VU.Unbox a) => SegTree s a -> Int -> Int -> ST s a
unsafeProdST SegTree {..} l0 r0 = inner (l0 + sizeSt) (r0 + sizeSt - 1) mempty mempty
  where
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

{-# INLINE updateST #-}
updateST :: (Monoid a, VU.Unbox a) => SegTree s a -> Int -> ST s ()
updateST SegTree {..} k = do
  opL <- VGM.read dSt $ 2 * k
  opR <- VGM.read dSt $ 2 * k + 1
  VGM.write dSt k $! opL <> opR
