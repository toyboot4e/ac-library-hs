{-# LANGUAGE RecordWildCards #-}

-- | Disjoint set union.
module AtCoder.Dsu (Dsu, new, merge, merge_, leader, same, size, groups) where

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
data Dsu s = Dsu
  { -- | The number of nodes.
    nDsu :: {-# UNPACK #-} !Int,
    -- | For root (leader) nodes it stores their size as a negative number. For child nodes it
    -- stores their parent node index.
    parentOrSizeDsu :: !(VUM.MVector s Int)
  }

-- | \(O(n)\)
new :: (PrimMonad m) => Int -> m (Dsu (PrimState m))
new nDsu
  | nDsu >= 0 = do
    parentOrSizeDsu <- VUM.replicate nDsu (-1)
    return Dsu {..}
  | otherwise = error $ "new: given negative size (`" ++ show nDsu ++ "`)"

-- | Amortized \(O(\alpha(n))\).
merge :: (HasCallStack, PrimMonad m) => Dsu (PrimState m) -> Int -> Int -> m Int
merge dsu@Dsu {..} a b = do
  let !_ = runtimeAssert (0 <= a && a < nDsu) $ "merge: vertex out of bounds (`" ++ show a ++ "` over the number of vertices `" ++ show nDsu ++ "`)"
  let !_ = runtimeAssert (0 <= b && b < nDsu) $ "merge: vertex out of bounds (`" ++ show b ++ "` over the number of vertices `" ++ show nDsu ++ "`)"
  x <- leader dsu a
  y <- leader dsu b
  if x == y
    then do
      return x
    else do
      px <- VGM.read parentOrSizeDsu x
      py <- VGM.read parentOrSizeDsu y
      when (-px < -py) $ do
        VGM.swap parentOrSizeDsu x y
      sizeY <- VGM.exchange parentOrSizeDsu y x
      VGM.modify parentOrSizeDsu (+ sizeY) x
      return x

-- | Amortized \(O(\alpha(n))\).
--
-- This function is not in the original ac-library. It's useful for suppressing warnings.
merge_ :: (PrimMonad m) => Dsu (PrimState m) -> Int -> Int -> m ()
merge_ dsu a b = do
  _ <- merge dsu a b
  return ()

-- | Amortized \(O(\alpha(n))\).
same :: (HasCallStack, PrimMonad m) => Dsu (PrimState m) -> Int -> Int -> m Bool
same dsu@Dsu {..} a b = do
  let !_ = runtimeAssert (0 <= a && a < nDsu) $ "same: vertex out of bounds (`" ++ show a ++ "` over the number of vertices `" ++ show nDsu ++ "`)"
  let !_ = runtimeAssert (0 <= b && b < nDsu) $ "same: vertex out of bounds (`" ++ show b ++ "` over the number of vertices `" ++ show nDsu ++ "`)"
  la <- leader dsu a
  lb <- leader dsu b
  return $ la == lb

-- | Amortized \(O(\alpha(n))\).
leader :: (HasCallStack, PrimMonad m) => Dsu (PrimState m) -> Int -> m Int
leader dsu@Dsu {..} a = do
  let !_ = runtimeAssert (0 <= a && a < nDsu) $ "leader: vertex out of bounds (`" ++ show a ++ "` over the number of vertices `" ++ show nDsu ++ "`)"
  pa <- VGM.read parentOrSizeDsu a
  if pa < 0
    then return a
    else do
      lpa <- leader dsu pa
      VGM.write parentOrSizeDsu a lpa
      return lpa

-- | Amortized \(O(\alpha(n))\).
size :: (HasCallStack, PrimMonad m) => Dsu (PrimState m) -> Int -> m Int
size dsu@Dsu {..} a = do
  let !_ = runtimeAssert (0 <= a && a < nDsu) $ "size: vertex out of bounds (`" ++ show a ++ "` over the number of vertices `" ++ show nDsu ++ "`)"
  la <- leader dsu a
  sizeLa <- VGM.read parentOrSizeDsu la
  return (-sizeLa)

-- | \(O(n \alpha(n))\)
groups :: (PrimMonad m) => Dsu (PrimState m) -> m (V.Vector (VU.Vector Int))
groups dsu@Dsu {..} = do
  groupSize <- VUM.replicate nDsu (0 :: Int)
  leaders <- VU.generateM nDsu $ \i -> do
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
