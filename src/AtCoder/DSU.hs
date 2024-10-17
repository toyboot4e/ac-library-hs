{-# LANGUAGE RecordWildCards #-}

-- | Disjoint set union.
module AtCoder.DSU (DSU, new, merge, merge_, leader, same, size, groups) where

import AtCoder.Internal.Assert (runtimeAssert)
import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)

-- | Disjoint set union.
data DSU s = DSU
  { -- | The number of nodes.
    nDSU :: {-# UNPACK #-} !Int,
    -- | For root (leader) nodes it stores their size as a negative number. For child nodes it
    -- stores their parent node index.
    parentOrSizeDSU :: !(VUM.MVector s Int)
  }

-- | \(O(n)\)
new :: (PrimMonad m) => Int -> m (DSU (PrimState m))
new nDSU = do
  parentOrSizeDSU <- VUM.replicate nDSU (-1)
  return DSU {..}

-- | Amortized \(O(\alpha(n))\).
merge :: (HasCallStack, PrimMonad m) => DSU (PrimState m) -> Int -> Int -> m Int
merge dsu@DSU {..} a b = do
  let !_ = runtimeAssert (0 <= a && a < nDSU) "merge: vertex out of bounds"
  let !_ = runtimeAssert (0 <= b && b < nDSU) "merge: vertex out of bounds"
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
--
-- This function is not in the original ac-library. It's useful for suppressing warnings.
merge_ :: (PrimMonad m) => DSU (PrimState m) -> Int -> Int -> m ()
merge_ dsu a b = do
  _ <- merge dsu a b
  return ()

-- | Amortized \(O(\alpha(n))\).
same :: (HasCallStack, PrimMonad m) => DSU (PrimState m) -> Int -> Int -> m Bool
same dsu@DSU {..} a b = do
  let !_ = runtimeAssert (0 <= a && a < nDSU) "same: vertex out of bounds"
  let !_ = runtimeAssert (0 <= b && b < nDSU) "same: vertex out of bounds"
  la <- leader dsu a
  lb <- leader dsu b
  return $ la == lb

-- | Amortized \(O(\alpha(n))\).
leader :: (HasCallStack, PrimMonad m) => DSU (PrimState m) -> Int -> m Int
leader dsu@DSU {..} a = do
  let !_ = runtimeAssert (0 <= a && a < nDSU) "leader: vertex out of bounds"
  pa <- VGM.read parentOrSizeDSU a
  if pa < 0
    then return a
    else do
      lpa <- leader dsu pa
      VGM.write parentOrSizeDSU a lpa
      return lpa

-- | Amortized \(O(\alpha(n))\).
size :: (HasCallStack, PrimMonad m) => DSU (PrimState m) -> Int -> m Int
size dsu@DSU {..} a = do
  let !_ = runtimeAssert (0 <= a && a < nDSU) "size: vertex out of bounds"
  la <- leader dsu a
  sizeLa <- VGM.read parentOrSizeDSU la
  return (-sizeLa)

-- | \(O(n \alpha(n))\)
groups :: (PrimMonad m) => DSU (PrimState m) -> m (V.Vector (VU.Vector Int))
groups dsu@DSU {..} = do
  groupSize <- VUM.replicate nDSU (0 :: Int)
  leaders <- VU.generateM nDSU $ \i -> do
    li <- leader dsu i
    VGM.modify groupSize (+ 1) li
    return li
  result <- do
    groupSize' <- VU.unsafeFreeze groupSize
    V.mapM VUM.unsafeNew $ VU.convert groupSize'
  VU.iforM_ leaders $ \i li -> do
    i' <- subtract 1 <$> VGM.read groupSize li
    VGM.write (result VG.! li) i' i
    VGM.write groupSize li i'
  V.filter (not . VU.null) <$> V.mapM VU.unsafeFreeze result
