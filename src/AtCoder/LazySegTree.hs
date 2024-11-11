{-# LANGUAGE RecordWildCards #-}

-- | Lazily propagted segment tree. It is the data structure for the pair of a [monoid](https://en.wikipedia.org/wiki/Monoid)
-- \((S, \cdot: S \times S \to S, e \in S)\) and a set \(F\) of \(S \to S\) mappings that satisfies
-- the following properties.
--
-- - \(F\) contains the identity map \(\mathrm{id}\), where the identity map is the map that
--   satisfies \(\mathrm{id}(x) = x\) for all \(x \in S\).
-- - \(F\) is closed under composition, i.e., \(f \circ g \in F\) holds for all \(f, g \in F\).
-- - \(f(x \cdot y) = f(x) \cdot f(y)\) holds for all \(f \in F\) and \(x, y \in S\).
--
-- Given an array \(S\) of length \(N\), it processes the following queries in \(O(\log N)\) time.
--
-- - Acting the map \(f\in F\) (cf. \(x = f(x)\)) on all the elements of an interval
-- - Calculating the product of the elements of an interval
--
-- For simplicity, in this document, we assume that the oracles @op@, @e@, @mapping@, @composition@,
-- and @id@ work in constant time. If these oracles work in \(O(T)\) time, each time complexity
-- appear in this document is multipled by \(O(T)\).
--
-- = Storing boxed types
-- If you really need to store boxed type to `LazySegTree`, wrap them in a newtype that implements
-- @Unbox@ typeclass via a boxed vector, such as [@DoNotUnboxStrict a@](https://hackage.haskell.org/package/vector-0.13.2.0/docs/Data-Vector-Unboxed.html#t:DoNotUnboxStrict).
module AtCoder.LazySegTree
  ( SegAct (..),
    LazySegTree (..),
    new,
    build,
    set,
    get,
    prod,
    allProd,
    applyAt,
    applyIn,
    maxRight,
    minLeft,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Bit qualified as ACIBIT
import Control.Monad (unless, when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bits (countTrailingZeros, testBit, (.&.), (.<<.), (.>>.))
import Data.Foldable (for_)
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)

-- | Haskell reprentation of the above properties.
class (Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => SegAct f a where
  -- | Lazy segment tree action.
  --
  -- = Constraints
  --
  -- - Left monoid action: @(f2 <> f1) a = f2 (f1 a)@
  -- - Endomorphism: @f (a1 <> a2) = (f a1) <> (f a2)@
  segAct :: f -> a -> a

data LazySegTree s f a = LazySegTree
  { -- | Valid length.
    nLst :: {-# UNPACK #-} !Int,
    -- | \(\lceil \log_2 \mathrm{nLst} \rceil\)
    sizeLst :: {-# UNPACK #-} !Int,
    -- | \(\log_2 \mathrm{sizeLst}\).
    logLst :: {-# UNPACK #-} !Int,
    -- | Data storage of length @2 * sizeLst@.
    dLst :: !(VUM.MVector s a),
    -- | Data storage of length @sizeLst@.
    lzLst :: !(VUM.MVector s f)
  }

-- | Creates an array of length @n@. All the elements are initialized to `mempty`.
--
-- = Constraints
-- - \(0 \leq n\)
--
-- = Complexity
-- - \(O(n)\)
new :: (HasCallStack, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a, PrimMonad m) => Int -> m (LazySegTree (PrimState m) f a)
new nLst
  | nLst >= 0 = build $ VU.replicate nLst mempty
  | otherwise = error $ "new: given negative size `" ++ show nLst ++ "`"

-- | Creates an array with initial values @vs@.
--
-- = Constraints
-- - \(0 \leq n\)
--
-- = Complexity
-- - \(O(n)\)
build :: (Monoid f, VU.Unbox f, Monoid a, VU.Unbox a, PrimMonad m) => VU.Vector a -> m (LazySegTree (PrimState m) f a)
build vs = do
  let nLst = VU.length vs
  let sizeLst = ACIBIT.bitCeil nLst
  let logLst = countTrailingZeros sizeLst
  dLst <- VUM.replicate (2 * sizeLst) mempty
  lzLst <- VUM.replicate sizeLst mempty
  VU.iforM_ vs $ \i v -> do
    VGM.write dLst (sizeLst + i) v
  let segtree = LazySegTree {..}
  for_ [sizeLst - 1, sizeLst - 2 .. 1] $ \i -> do
    update segtree i
  return segtree

-- | Sets \(p\)-th value of the array to \(x\).
--
-- = Constraints
-- - \(0 \leq p \lt n\)
--
-- = Complexity
-- - \(O(\log n)\)
set :: (HasCallStack, PrimMonad m, SegAct f a) => LazySegTree (PrimState m) f a -> Int -> a -> m ()
set self@LazySegTree {..} p x = do
  let !_ = ACIA.checkIndex "AtCoder.LazySegTree.set" p nLst
  let p' = p + sizeLst
  for_ [logLst, logLst - 1 .. 1] $ \i -> do
    push self $ p' .>>. i
  VGM.write dLst p' x
  for_ [1 .. logLst] $ \i -> do
    update self $ p' .>>. i

-- | Returns \(p\)-th value of the array.
--
-- = Constraints
-- - \(0 \leq p \lt n\)
--
-- = Complexity
-- - \(O(\log n)\)
get :: (HasCallStack, PrimMonad m, SegAct f a) => LazySegTree (PrimState m) f a -> Int -> m a
get self@LazySegTree {..} p = do
  let !_ = ACIA.checkIndex "AtCoder.LazySegTree.get" p nLst
  let p' = p + sizeLst
  for_ [logLst, logLst - 1 .. 1] $ \i -> do
    push self $ p' .>>. i
  VGM.read dLst p'

-- | Returns the product of \([a[l], ..., a[r - 1]]\), assuming the properties of the monoid. It
-- returns `mempty` if \(l = r\).
--
-- = Constraints
-- - \(0 \leq l \leq r \leq n\)
--
-- = Complexity
-- - \(O(\log n)\)
prod :: (HasCallStack, PrimMonad m, SegAct f a) => LazySegTree (PrimState m) f a -> Int -> Int -> m a
prod self@LazySegTree {..} l0 r0
  | l0 == r0 = return mempty
  | otherwise = do
      let l = l0 + sizeLst
      let r = r0 + sizeLst
      for_ [logLst, logLst - 1 .. 1] $ \i -> do
        when (((l .>>. i) .<<. i) /= l) $ push self $ l .>>. i
        when (((r .>>. i) .<<. i) /= r) $ push self $ (r - 1) .>>. i
      inner l (r - 1) mempty mempty
  where
    !_ = ACIA.checkInterval "AtCoder.LazySegTree.prod" l0 r0 nLst
    -- NOTE: we're using inclusive range [l, r] for simplicity
    inner l r !smL !smR
      | l > r = return $! smL <> smR
      | otherwise = do
          smL' <-
            if testBit l 0
              then (smL <>) <$> VGM.read dLst l
              else return smL
          smR' <-
            if not $ testBit r 0
              then (<> smR) <$> VGM.read dLst r
              else return smR
          inner ((l + 1) .>>. 1) ((r - 1) .>>. 1) smL' smR'

-- | Returns the product of \([op(a[0], ..., a[n - 1])]\), assuming the properties of the monoid. It
-- returns `mempty` if \(n = 0\).
--
-- = Complexity
-- - \(O(1)\)
allProd :: (PrimMonad m, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> m a
allProd LazySegTree {..} = VGM.read dLst 1

-- | Applies @segAct f@ to an index @p@.
--
-- = Constraints
-- - \(0 \leq p \lt n\)
--
-- = Complexity
-- - \(O(\log n)\)
applyAt :: (HasCallStack, PrimMonad m, SegAct f a) => LazySegTree (PrimState m) f a -> Int -> f -> m ()
applyAt self@LazySegTree {..} p f = do
  let !_ = ACIA.checkIndex "AtCoder.LazySegTree.applyAt" p nLst
  let p' = p + sizeLst
  -- propagate
  for_ [logLst, logLst - 1 .. 1] $ \i -> do
    push self $ p' .>>. i
  VGM.modify dLst (f `segAct`) p'
  -- evaluate
  for_ [1 .. logLst] $ \i -> do
    update self $ p' .>>. i

-- | Applies @segAct f@ to an interval @[l, r)@.
--
-- = Constraints
-- - \(0 \leq l \leq r \leq n\)
--
-- = Complexity
-- - \(O(\log n)\)
applyIn :: (HasCallStack, PrimMonad m, SegAct f a) => LazySegTree (PrimState m) f a -> Int -> Int -> f -> m ()
applyIn self@LazySegTree {..} l0 r0 f
  | l0 == r0 = return ()
  | otherwise = do
      let l = l0 + sizeLst
      let r = r0 + sizeLst
      -- propagate
      for_ [logLst, logLst - 1 .. 1] $ \i -> do
        when (((l .>>. i) .<<. i) /= l) $ push self (l .>>. i)
        when (((r .>>. i) .<<. i) /= r) $ push self ((r - 1) .>>. i)
      inner l (r - 1)
      -- evaluate
      for_ [1 .. logLst] $ \i -> do
        when (((l .>>. i) .<<. i) /= l) $ update self (l .>>. i)
        when (((r .>>. i) .<<. i) /= r) $ update self ((r - 1) .>>. i)
  where
    !_ = ACIA.checkInterval "AtCoder.LazySegTree.applyIn" l0 r0 nLst
    -- NOTE: we're using inclusive range [l, r] for simplicity
    inner l r
      | l > r = return ()
      | otherwise = do
          when (testBit l 0) $ do
            allApply self l f
          unless (testBit r 0) $ do
            allApply self r f
          inner ((l + 1) .>>. 1) ((r - 1) .>>. 1)

-- | Applies a binary search on the segment tree. It returns an index @r@ that satisfies both of the
-- followings.
--
-- - @r = l@ or @g(a[l] <> a[l + 1] <> ... <> a[r - 1])) == True@
-- - @r = n@ or @g(a[l] <> a[l + 1] <> ... <> a[r])) == False@
--
-- If @g@ is monotone, this is the maximum @r@ that satisfies @g(a[l] <> a[l + 1] <> ... <> a[r - 1]) == True@.
--
-- = Constraints
--
-- - @g mempty == True@
-- - \(0 \leq l \leq n\)
--
-- = Complexity
-- - \(O(\log n)\)
maxRight :: (HasCallStack, PrimMonad m, SegAct f a) => LazySegTree (PrimState m) f a -> Int -> (a -> Bool) -> m Int
maxRight self@LazySegTree {..} l0 g
  | l0 == nLst = return nLst
  | otherwise = do
      let l = l0 + sizeLst
      for_ [logLst, logLst - 1 .. 1] $ \i -> do
        push self (l .>>. i)
      inner l mempty
  where
    -- NOTE: Not ordinary bounds check!
    !_ = ACIA.runtimeAssert (0 <= l0 && l0 <= nLst) $ "AtCoder.LazySegTree.maxRight: given invalid `left` index `" ++ show l0 ++ "` over length `" ++ show nLst ++ "`"
    !_ = ACIA.runtimeAssert (g mempty) "AtCoder.LazySegTree.maxRight: `g mempty` returned `False`"
    inner l !sm = do
      let l' = chooseBit l
      !sm' <- (sm <>) <$> VGM.read dLst l'
      if not $ g sm'
        then do
          inner2 l' sm
        else do
          let l'' = l' + 1
          if (l'' .&. (-l'')) /= l''
            then inner l'' sm'
            else return nLst
    chooseBit :: Int -> Int
    chooseBit l
      | even l = chooseBit $ l .>>. 1
      | otherwise = l
    inner2 l !sm
      | l < sizeLst = do
          push self l
          let l' = 2 * l
          !sm' <- (sm <>) <$> VGM.read dLst l'
          if g sm'
            then inner2 (l' + 1) sm'
            else inner2 l' sm
      | otherwise = return $ l - sizeLst

-- | Applies a binary search on the segment tree. It returns an index @l@ that satisfies both of the
-- following.
--
-- - @l = r@ or @g(a[l] <> a[l + 1] <> ... <> a[r - 1]) == True@
-- - @l = 0@ or @g(a[l - 1] <> a[l] <> ... <> a[r - 1]) == False@
--
-- If @g@ is monotone, this is the minimum @l@ that satisfies @g(a[l] <> a[l + 1] <> ... <> a[r - 1]) == True@.
--
-- = Constraints
--
-- - @g mempty == True@
-- - \(0 \leq r \leq n\)
--
-- = Complexity
-- - \(O(\log n)\)
minLeft :: (HasCallStack, PrimMonad m, SegAct f a) => LazySegTree (PrimState m) f a -> Int -> (a -> Bool) -> m Int
minLeft self@LazySegTree {..} r0 g
  | r0 == 0 = return 0
  | otherwise = do
      let r = r0 + sizeLst
      for_ [logLst, logLst - 1 .. 1] $ \i -> do
        push self $ (r - 1) .>>. i
      inner r mempty
  where
    -- NOTE: Not ordinary bounds check!
    !_ = ACIA.runtimeAssert (0 <= r0 && r0 <= nLst) $ "AtCoder.LazySegTree.minLeft: given invalid `right` index `" ++ show r0 ++ "` over length `" ++ show nLst ++ "`"
    !_ = ACIA.runtimeAssert (g mempty) "AtCoder.LazySegTree.minLeft: `g empty` returned `False`"
    inner r !sm = do
      let r' = chooseBit $ r - 1
      !sm' <- (<> sm) <$> VGM.read dLst r'
      if not $ g sm'
        then do
          inner2 r' sm
        else do
          if (r' .&. (-r')) /= r'
            then inner r' sm'
            else return 0
    chooseBit r
      | r > 1 && odd r = chooseBit $ r .>>. 1
      | otherwise = r
    inner2 r sm
      | r < sizeLst = do
          push self r
          let r' = 2 * r + 1
          !sm' <- (<> sm) <$> VGM.read dLst r'
          if g sm'
            then inner2 (r' - 1) sm'
            else inner2 r' sm
      | otherwise = return $ r + 1 - sizeLst

-- | \(O(1)\)
update :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a, Monoid f, VU.Unbox f) => LazySegTree (PrimState m) f a -> Int -> m ()
update LazySegTree {..} k = do
  opL <- VGM.read dLst $ 2 * k
  opR <- VGM.read dLst $ 2 * k + 1
  VGM.write dLst k $! opL <> opR

-- | \(O(1)\)
allApply :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a, Monoid f, VU.Unbox f, SegAct f a) => LazySegTree (PrimState m) f a -> Int -> f -> m ()
allApply LazySegTree {..} k f = do
  VGM.modify dLst (f `segAct`) k
  when (k < sizeLst) $ do
    VGM.modify lzLst (f <>) k

-- | \(O(1)\)
push :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a, Monoid f, VU.Unbox f, SegAct f a) => LazySegTree (PrimState m) f a -> Int -> m ()
push self@LazySegTree {..} k = do
  lzK <- VGM.read lzLst k
  allApply self (2 * k) lzK
  allApply self (2 * k + 1) lzK
  VGM.write lzLst k mempty
