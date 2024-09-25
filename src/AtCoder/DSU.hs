{-# LANGUAGE RecordWildCards #-}

-- | Disjoint set union.
module AtCoder.DSU (DSU, new, merge, same, size, groups) where

import Control.Exception (assert)
import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

-- | Disjoint set union.
data DSU s = DSU
  { nDSU :: {-# UNPACK #-} !Int,
    parentOrSizeDSU :: !(VUM.MVector s Int)
  }

-- | \(O(n)\)
new :: (PrimMonad m) => Int -> m (DSU (PrimState m))
new nDSU = do
  parentOrSizeDSU <- VUM.replicate nDSU (-1)
  return DSU {..}

-- | Amortized \(O(\alpha(n))\).
merge :: (PrimMonad m) => DSU (PrimState m) -> Int -> Int -> m Int
merge dsu@DSU {..} a b = do
  let !_ = assert (0 <= a && a < nDSU) ()
  let !_ = assert (0 <= b && b < nDSU) ()
  x <- leader dsu a
  y <- leader dsu b
  if x == y
    then do
      return x
    else do
      px <- VGM.read parentOrSizeDSU x
      py <- VGM.read parentOrSizeDSU y
      when (-px < -py) $ do
        VGM.swap parentOrSizeDSU x y
      sizeY <- VGM.exchange parentOrSizeDSU y x
      VGM.modify parentOrSizeDSU (+ sizeY) x
      return x

-- | Amortized \(O(\alpha(n))\).
same :: (PrimMonad m) => DSU (PrimState m) -> Int -> Int -> m Bool
same dsu@DSU {..} a b = do
  let !_ = assert (0 <= a && a < nDSU) ()
  let !_ = assert (0 <= b && b < nDSU) ()
  la <- leader dsu a
  lb <- leader dsu b
  return $ la == lb

-- | Amortized \(O(\alpha(n))\).
leader :: (PrimMonad m) => DSU (PrimState m) -> Int -> m Int
leader dsu@DSU {..} a = do
  let !_ = assert (0 <= a && a < nDSU) ()
  pa <- VGM.read parentOrSizeDSU a
  if pa < 0
    then return a
    else do
      lpa <- leader dsu pa
      VGM.write parentOrSizeDSU a lpa
      return lpa

-- | Amortized \(O(\alpha(n))\).
size :: (PrimMonad m) => DSU (PrimState m) -> Int -> m Int
size dsu@DSU {..} a = do
  let !_ = assert (0 <= a && a < nDSU) ()
  la <- leader dsu a
  pla <- VGM.read parentOrSizeDSU la
  return (-pla)

-- | \(O(n)\)
groups :: (PrimMonad m) => DSU (PrimState m) -> m (V.Vector (VU.Vector Int))
groups dsu@DSU {..} = do
  groupSize <- VUM.replicate nDSU (0 :: Int)
  leaders <- VU.generateM nDSU $ \i -> do
    li <- leader dsu i
    VGM.modify groupSize (+ 1) li
    return li
  result <- V.mapM VUM.unsafeNew $ VU.convert leaders
  VU.iforM_ leaders $ \i li -> do
    sizeLI <- VGM.read groupSize li
    VGM.write (result VG.! i) (sizeLI - 1) i
    VGM.write groupSize li (sizeLI - 1)
  V.filter (not . VU.null) <$> V.mapM VU.unsafeFreeze result
