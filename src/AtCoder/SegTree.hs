{-# LANGUAGE RecordWildCards #-}

-- | Segment tree.
--
-- = Storing boxed types
-- If you really need to store boxed type to `SegTree`, wrap them in a newtype that implements
-- @Unbox@ typeclass via boxed vector, such as [@DoNotUnboxStrict a@](https://hackage.haskell.org/package/vector-0.13.2.0/docs/Data-Vector-Unboxed.html#t:DoNotUnboxStrict).
module AtCoder.SegTree
  ( SegTree (..),
    new,
    build,
    set,
    get,
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
import Data.Foldable
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack

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

new :: (HasCallStack, Monoid a, VU.Unbox a, PrimMonad m) => Int -> m (SegTree (PrimState m) a)
new nSt
  | nSt >= 0 = build $ VU.replicate nSt mempty
  | otherwise = error $ "new: given negative size (`" ++ show nSt ++ "`)"

build :: (Monoid a, VU.Unbox a, PrimMonad m) => VU.Vector a -> m (SegTree (PrimState m) a)
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
  return segtree

set :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => SegTree (PrimState m) a -> Int -> a -> m ()
set self@SegTree {..} p x = do
  let !_ = ACIA.checkIndex "AtCoder.SegTree.set" p nSt
  VGM.write dSt (p + sizeSt) x
  for_ [1 .. logSt] $ \i -> do
    update self ((p + sizeSt) .>>. i)

get :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => SegTree (PrimState m) a -> Int -> m a
get SegTree {..} p = do
  let !_ = ACIA.checkIndex "AtCoder.SegTree.get" p nSt
  VGM.read dSt $ p + sizeSt

prod :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => SegTree (PrimState m) a -> Int -> Int -> m a
prod SegTree {..} l0 r0 = inner (l0 + sizeSt) (r0 + sizeSt - 1) mempty mempty
  where
    !_ = ACIA.checkInterval "AtCoder.SegTree.prod" l0 r0 nSt
    -- NOTE: we're using inclusive range [l, r] for simplicity
    inner l r !smL !smR
      | l > r = return $! smL <> smR
      | otherwise = do
          smL' <-
            if testBit l 0
              then (smL <>) <$> VGM.read dSt l
              else return smL
          smR' <-
            if not $ testBit r 0
              then (<> smR) <$> VGM.read dSt r
              else return smR
          inner ((l + 1) .>>. 1) ((r - 1) .>>. 1) smL' smR'

allProd :: (PrimMonad m, Monoid a, VU.Unbox a) => SegTree (PrimState m) a -> m a
allProd SegTree {..} = VGM.read dSt 1

maxRight :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => SegTree (PrimState m) a -> Int -> (a -> Bool) -> m Int
maxRight SegTree {..} l0 f
  | l0 == nSt = return nSt
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
            else return nSt
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
      | otherwise = return $ l - sizeSt

minLeft :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => SegTree (PrimState m) a -> Int -> (a -> Bool) -> m Int
minLeft SegTree {..} r0 f
  | r0 == 0 = return 0
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
            else return 0
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
      | otherwise = return $ r + 1 - sizeSt

update :: (PrimMonad m, Monoid a, VU.Unbox a) => SegTree (PrimState m) a -> Int -> m ()
update SegTree {..} k = do
  opL <- VGM.read dSt $ 2 * k
  opR <- VGM.read dSt $ 2 * k + 1
  VGM.write dSt k $! opL <> opR
