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
-- If you really need to store boxed types to `LazySegTree`, use [@DoNotUnboxStrict a@](https://hackage.haskell.org/package/vector-0.13.2.0/docs/Data-Vector-Unboxed.html#t:DoNotUnboxStrict)
-- or other wrappers.
--
-- = Example
--
-- == Usage
-- Here, we're using `AtCoder.Extra.Monoid.Affine2d`.
--
-- >>> import AtCoder.LazySegTree qualified as LST
-- >>> import AtCoder.Extra.Monoid (SegAct(..), Affine2d(..))
-- >>> import Data.Semigroup (Sum(..))
-- >>> seg <- LST.build @_ @(Affine2d Int) @(Sum Int) $ VU.fromList [1, 2, 3, 4]
-- >>> LST.applyIn seg 1 3 $ Affine2d (2, 1) -- \x -> 2 * x + 1
-- >>> LST.write seg 3 $ Sum 10 -- [1, 5, 7, 10]
-- >>> LST.modify seg (+ 1) 0   -- [2, 5, 7, 10]
-- >>> LST.read seg 1
-- Sum {getSum = 5}
-- >>> LST.prod seg 0 3
-- Sum {getSum = 14}
-- >>> LST.allProd seg
-- Sum {getSum = 24}
-- >>> LST.maxRight seg 0 (<= (Sum 10)) -- sum [0, 2) = 7 <= 10
-- 2
-- >>> LST.minLeft seg 4 (<= (Sum 10)) -- sum [3, 4) = 10 <= 10
-- 3
--
-- == `SegAct` instance
-- `LazeSegTree` functions require `SegAct` instance. Take `AtCoder.Extra.Monoid.Affine2d` as an
-- example.
--
-- @
-- {-# LANGUAGE TypeFamilies #-}
--
-- import AtCoder.LazySegTree (SegAct (..)) -- also re-exported from AtCoder.Extra.Monoid.
-- import Data.Monoid
-- import Data.Vector.Generic qualified as VG
-- import Data.Vector.Generic.Mutable qualified as VGM
-- import Data.Vector.Unboxed qualified as VU
-- import Data.Vector.Unboxed.Mutable qualified as VUM
--
-- newtype Affine2d a = Affine2d (Affine2dRepr a)
--   deriving newtype (Eq, Ord, Show)
--
-- type Affine2dRepr a = (a, a)
--
-- instance (Num a) => 'Semigroup' (Affine2d a) where
--   {-# INLINE ('<>') #-}
--   (Affine2d (!a1, !b1)) '<>' (Affine2d (!a2, !b2)) = Affine2d (a1 * a2, a1 * b2 + b1)
--
-- instance (Num a) => 'Monoid' (Affine2d a) where
--   {-# INLINE 'mempty' #-}
--   'mempty' = Affine2d (1, 0)
--   {-# INLINE mconcat #-}
--   mconcat = foldl' (<>) mempty
--
-- instance (Integral a) => SegAct (Affine2d a) (Sum a) where
--   {-# INLINE 'segActWithLength' #-}
--   'segActWithLength' !len (Affine2d (!a, !b)) (Sum !x) = Sum $ a * x + b * fromIntegral len
--
-- -- Other 'SegAct' instances are ommited
--
-- -- Derive Unbox:
-- newtype instance VU.MVector s (Affine2d a) = MV_Affine2d (VU.MVector s (Affine2dRepr a))
-- newtype instance VU.Vector (Affine2d a) = V_Affine2d (VU.Vector (Affine2dRepr a))
-- deriving instance (VU.Unbox a) => VGM.MVector VUM.MVector (Affine2d a)
-- deriving instance (VU.Unbox a) => VG.Vector VU.Vector (Affine2d a)
-- instance (VU.Unbox a) => VU.Unbox (Affine2d a)
-- @
--
-- Tips:
--
-- - New monoid always come from the left: @new <> old@.
-- - `prod` returns \(a_l \diamond f_{l + 1} \diamond .. \diamond a_{r - 1}\).
-- - If you need \(a_{r - 1} \diamond a_{r - 2} \diamond .. \diamond f_{l}\), wrap your monoid in `Data.Monoid.Dual`.
--
-- = Major changes from the original @ac-library@
-- - The implementaion is `Monoid` and `SegAct` based.
-- - @get@ and @set@ are renamed to `read` and `write`.
-- - `modify` and `modifyM` are added.
module AtCoder.LazySegTree
  ( SegAct (..),
    LazySegTree (..),
    new,
    build,
    write,
    modify,
    modifyM,
    read,
    prod,
    allProd,
    applyAt,
    applyIn,
    maxRight,
    maxRightM,
    minLeft,
    minLeftM,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Bit qualified as ACIBIT
import Control.Monad (unless, when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bits (bit, countLeadingZeros, countTrailingZeros, testBit, (.&.), (.<<.), (.>>.))
import Data.Foldable (for_)
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)
import Prelude hiding (read)

-- | Haskell reprentation of the `AtCoder.LazySegTree` properties. User can implement either
-- `segAct` or `segActWithLength`.
class (Monoid f) => SegAct f a where
  -- | Lazy segment tree action.
  --
  -- = Constraints
  --
  -- - Left monoid action: \((f_2 \diamond f_1) a = f_2 (f_1 a)\)
  -- - Endomorphism: \(f (a_1 \diamond a_2) = (f a_1) \diamond (f a_2)\)
  {-# INLINE segAct #-}
  segAct :: f -> a -> a
  segAct = segActWithLength 1

  -- | Lazy segment tree action with target monoid length.
  --
  -- If you implement `SegAt` with this function, you don't have to store the monoid length, since
  -- it's given externally.
  {-# INLINE segActWithLength #-}
  segActWithLength :: Int -> f -> a -> a
  segActWithLength _ = segAct

-- | Lazy segment tree defined around `SegAct`.
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
new :: (HasCallStack, PrimMonad m, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Int -> m (LazySegTree (PrimState m) f a)
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
build :: (PrimMonad m, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => VU.Vector a -> m (LazySegTree (PrimState m) f a)
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
  pure segtree

-- | Sets \(p\)-th value of the array to \(x\).
--
-- = Constraints
-- - \(0 \leq p \lt n\)
--
-- = Complexity
-- - \(O(\log n)\)
write :: (HasCallStack, PrimMonad m, SegAct f a, VU.Unbox f, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> Int -> a -> m ()
write self@LazySegTree {..} p x = do
  let !_ = ACIA.checkIndex "AtCoder.LazySegTree.write" p nLst
  let p' = p + sizeLst
  for_ [logLst, logLst - 1 .. 1] $ \i -> do
    push self $ p' .>>. i
  VGM.write dLst p' x
  for_ [1 .. logLst] $ \i -> do
    update self $ p' .>>. i

-- | (Extra API) Modifies \(p\)-th value of the array to \(x\).
--
-- = Constraints
-- - \(0 \leq p \lt n\)
--
-- = Complexity
-- - \(O(\log n)\)
modify :: (HasCallStack, PrimMonad m, SegAct f a, VU.Unbox f, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> (a -> a) -> Int -> m ()
modify self@LazySegTree {..} f p = do
  let !_ = ACIA.checkIndex "AtCoder.LazySegTree.modify" p nLst
  let p' = p + sizeLst
  for_ [logLst, logLst - 1 .. 1] $ \i -> do
    push self $ p' .>>. i
  VGM.modify dLst f p'
  for_ [1 .. logLst] $ \i -> do
    update self $ p' .>>. i

-- | (Extra API) Modifies \(p\)-th value of the array to \(x\).
--
-- = Constraints
-- - \(0 \leq p \lt n\)
--
-- = Complexity
-- - \(O(\log n)\)
modifyM :: (HasCallStack, PrimMonad m, SegAct f a, VU.Unbox f, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> (a -> m a) -> Int -> m ()
modifyM self@LazySegTree {..} f p = do
  let !_ = ACIA.checkIndex "AtCoder.LazySegTree.modify" p nLst
  let p' = p + sizeLst
  for_ [logLst, logLst - 1 .. 1] $ \i -> do
    push self $ p' .>>. i
  VGM.modifyM dLst f p'
  for_ [1 .. logLst] $ \i -> do
    update self $ p' .>>. i

-- | Returns \(p\)-th value of the array.
--
-- = Constraints
-- - \(0 \leq p \lt n\)
--
-- = Complexity
-- - \(O(\log n)\)
read :: (HasCallStack, PrimMonad m, SegAct f a, VU.Unbox f, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> Int -> m a
read self@LazySegTree {..} p = do
  let !_ = ACIA.checkIndex "AtCoder.LazySegTree.read" p nLst
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
prod :: (HasCallStack, PrimMonad m, SegAct f a, VU.Unbox f, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> Int -> Int -> m a
prod self@LazySegTree {..} l0 r0
  | l0 == r0 = pure mempty
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
      | l > r = pure $! smL <> smR
      | otherwise = do
          !smL' <-
            if testBit l 0
              then (smL <>) <$> VGM.read dLst l
              else pure smL
          !smR' <-
            if not $ testBit r 0
              then (<> smR) <$> VGM.read dLst r
              else pure smR
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
applyAt :: (HasCallStack, PrimMonad m, SegAct f a, VU.Unbox f, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> Int -> f -> m ()
applyAt self@LazySegTree {..} p f = do
  let !_ = ACIA.checkIndex "AtCoder.LazySegTree.applyAt" p nLst
  let p' = p + sizeLst
  -- propagate
  for_ [logLst, logLst - 1 .. 1] $ \i -> do
    push self $ p' .>>. i
  let !len = bit $! logLst - (63 - countLeadingZeros p')
  VGM.modify dLst (segActWithLength len f) p'
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
applyIn :: (HasCallStack, PrimMonad m, SegAct f a, VU.Unbox f, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> Int -> Int -> f -> m ()
applyIn self@LazySegTree {..} l0 r0 f
  | l0 == r0 = pure ()
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
      | l > r = pure ()
      | otherwise = do
          when (testBit l 0) $ do
            allApply self l f
          unless (testBit r 0) $ do
            allApply self r f
          inner ((l + 1) .>>. 1) ((r - 1) .>>. 1)

-- | Applies a binary search on the segment tree. It returns an index \(r\) that satisfies both of the
-- followings.
--
-- - \(r = l\) or \(g(a[l] \diamond a[l + 1] \diamond ... \diamond a[r - 1]))\) returns `True`.
-- - \(r = n\) or \(g(a[l] \diamond a[l + 1] \diamond ... \diamond a[r]))\) returns `False`.
--
-- If \(g\) is monotone, this is the maximum \(r\) that satisfies
-- \(g(a[l] \diamond a[l + 1] \diamond ... \diamond a[r - 1])\).
--
-- = Constraints
--
-- - If \(g\) is called with the same argument, it returns the same value, i.e., \(g\) has no side effect.
-- - @g mempty == True@.
-- - \(0 \leq l \leq n\)
--
-- = Complexity
-- - \(O(\log n)\)
maxRight :: (HasCallStack, PrimMonad m, SegAct f a, VU.Unbox f, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> Int -> (a -> Bool) -> m Int
maxRight seg l0 g = maxRightM seg l0 (pure . g)

-- | Monadic version of `maxRight`.
--
-- = Constraints
--
-- - If \(g\) is called with the same argument, it returns the same value, i.e., \(g\) has no side effect.
-- - @g mempty == True@.
-- - \(0 \leq l \leq n\)
--
-- = Complexity
-- - \(O(\log n)\)
maxRightM :: (HasCallStack, PrimMonad m, SegAct f a, VU.Unbox f, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> Int -> (a -> m Bool) -> m Int
maxRightM self@LazySegTree {..} l0 g = do
  b <- g mempty
  let !_ = ACIA.runtimeAssert b "AtCoder.LazySegTree.maxRightM: `g mempty` returned `False`"
  if l0 == nLst
    then pure nLst
    else do
      let l = l0 + sizeLst
      for_ [logLst, logLst - 1 .. 1] $ \i -> do
        push self (l .>>. i)
      inner l mempty
  where
    -- NOTE: Not ordinary bounds check!
    !_ = ACIA.runtimeAssert (0 <= l0 && l0 <= nLst) $ "AtCoder.LazySegTree.maxRightM: given invalid `left` index `" ++ show l0 ++ "` over length `" ++ show nLst ++ "`"
    inner l !sm = do
      let l' = chooseBit l
      !sm' <- (sm <>) <$> VGM.read dLst l'
      b <- g sm'
      if not $ b
        then do
          inner2 l' sm
        else do
          let l'' = l' + 1
          if (l'' .&. (-l'')) /= l''
            then inner l'' sm'
            else pure nLst
    chooseBit :: Int -> Int
    chooseBit l
      | even l = chooseBit $ l .>>. 1
      | otherwise = l
    inner2 l !sm
      | l < sizeLst = do
          push self l
          let l' = 2 * l
          !sm' <- (sm <>) <$> VGM.read dLst l'
          b <- g sm'
          if b
            then inner2 (l' + 1) sm'
            else inner2 l' sm
      | otherwise = pure $ l - sizeLst

-- | Applies a binary search on the segment tree. It returns an index \(l\) that satisfies both of the
-- following.
--
-- - \(l = r\) or \(g(a[l] \diamond a[l + 1] \diamond ... \diamond a[r - 1])\) returns `True`.
-- - \(l = 0\) or \(g(a[l - 1] \diamond a[l] \diamond ... \diamond a[r - 1])\) returns `False`.
--
-- If \(g\) is monotone, this is the minimum \(l\) that satisfies
-- \(g(a[l] \diamond a[l + 1] \diamond ... \diamond a[r - 1])\).
--
-- = Constraints
--
-- - if \(g\) is called with the same argument, it returns the same value, i.e., \(g\) has no side effect.
-- - @g mempty == True@.
-- - \(0 \leq r \leq n\)
--
-- = Complexity
-- - \(O(\log n)\)
minLeft :: (HasCallStack, PrimMonad m, SegAct f a, VU.Unbox f, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> Int -> (a -> Bool) -> m Int
minLeft seg r0 g = minLeftM seg r0 (pure . g)

-- | Monadic version of `minLeft`.
--
-- = Constraints
--
-- - if \(g\) is called with the same argument, it returns the same value, i.e., \(g\) has no side effect.
-- - @g mempty == True@.
-- - \(0 \leq r \leq n\)
--
-- = Complexity
-- - \(O(\log n)\)
minLeftM :: (HasCallStack, PrimMonad m, SegAct f a, VU.Unbox f, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> Int -> (a -> m Bool) -> m Int
minLeftM self@LazySegTree {..} r0 g = do
  b <- g mempty
  let !_ = ACIA.runtimeAssert b "AtCoder.LazySegTree.minLeftM: `g empty` returned `False`"
  if r0 == 0
    then pure 0
    else do
      let r = r0 + sizeLst
      for_ [logLst, logLst - 1 .. 1] $ \i -> do
        push self $ (r - 1) .>>. i
      inner r mempty
  where
    -- NOTE: Not ordinary bounds check!
    !_ = ACIA.runtimeAssert (0 <= r0 && r0 <= nLst) $ "AtCoder.LazySegTree.minLeftM: given invalid `right` index `" ++ show r0 ++ "` over length `" ++ show nLst ++ "`"
    inner r !sm = do
      let r' = chooseBit $ r - 1
      !sm' <- (<> sm) <$> VGM.read dLst r'
      b <- g sm'
      if not $ b
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
      | r < sizeLst = do
          push self r
          let r' = 2 * r + 1
          !sm' <- (<> sm) <$> VGM.read dLst r'
          b <- g sm'
          if b
            then inner2 (r' - 1) sm'
            else inner2 r' sm
      | otherwise = pure $ r + 1 - sizeLst

-- | \(O(1)\)
update :: (HasCallStack, PrimMonad m, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> Int -> m ()
update LazySegTree {..} k = do
  opL <- VGM.read dLst $ 2 * k
  opR <- VGM.read dLst $ 2 * k + 1
  VGM.write dLst k $! opL <> opR

-- | \(O(1)\)
allApply :: (HasCallStack, PrimMonad m, SegAct f a, VU.Unbox f, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> Int -> f -> m ()
allApply LazySegTree {..} k f = do
  let !len = bit $! logLst - (63 - countLeadingZeros k)
  VGM.modify dLst (segActWithLength len f) k
  when (k < sizeLst) $ do
    VGM.modify lzLst (f <>) k

-- | \(O(1)\)
push :: (HasCallStack, PrimMonad m, SegAct f a, VU.Unbox f, Monoid a, VU.Unbox a) => LazySegTree (PrimState m) f a -> Int -> m ()
push self@LazySegTree {..} k = do
  lzK <- VGM.read lzLst k
  allApply self (2 * k) lzK
  allApply self (2 * k + 1) lzK
  VGM.write lzLst k mempty
