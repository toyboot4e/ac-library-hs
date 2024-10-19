{-# LANGUAGE RecordWildCards #-}

-- | Growable vector with indirection and runtime overhead.
module AtCoder.Internal.GrowVec
  ( GrowVec (..),
    new,
    build,
    read,
    pushBack,
    popBack,
    popBack_,
    length,
    null,
    freeze,
    unsafeFreeze,
  )
where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Primitive.MutVar (MutVar, newMutVar, readMutVar, writeMutVar)
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Prelude hiding (length, null, read)

-- | Growable vector with indirection and runtime overhead.
data GrowVec s a = GrowVec
  { -- | Stores [l, r) range in the `vecGV`.
    posGV :: !(VUM.MVector s Int),
    vecGV :: !(MutVar s (VUM.MVector s a))
  }

-- | \(O(n)\)
new :: (PrimMonad m, VU.Unbox a) => Int -> m (GrowVec (PrimState m) a)
new n = do
  posGV <- VUM.replicate 1 (0 :: Int)
  vecGV <- newMutVar =<< VUM.unsafeNew n
  return GrowVec {..}

-- | \(O(n)\)
build :: (PrimMonad m, VU.Unbox a) => VU.Vector a -> m (GrowVec (PrimState m) a)
build xs = do
  posGV <- VUM.replicate 1 $ VU.length xs
  vecGV <- newMutVar =<< VU.thaw xs
  return GrowVec {..}

-- | \(O(1)\)
read :: (PrimMonad m, VU.Unbox a) => GrowVec (PrimState m) a -> Int -> m a
read GrowVec {..} i = do
  vec <- readMutVar vecGV
  VGM.read vec i

-- | Amortized \(O(1)\). Grow the capacity twice
pushBack :: (PrimMonad m, VU.Unbox a) => GrowVec (PrimState m) a -> a -> m ()
pushBack GrowVec {..} e = do
  len <- VGM.read posGV 0
  vec <- do
    vec <- readMutVar vecGV
    if VUM.length vec > len
      then return vec
      else do
        newVec <- VUM.unsafeGrow vec $ max 1 len
        writeMutVar vecGV newVec
        return newVec

  VGM.modifyM
    posGV
    ( \r -> do
        VGM.write vec r e
        return $ r + 1
    )
    0

-- | \(O(1)\).
popBack :: (PrimMonad m, VU.Unbox a) => GrowVec (PrimState m) a -> m (Maybe a)
popBack GrowVec {..} = do
  pos <- VGM.read posGV 0
  if pos <= 0
    then return Nothing
    else do
      VGM.write posGV 0 $ pos - 1
      vec <- readMutVar vecGV
      Just <$> VGM.read vec (pos - 1)

-- | \(O(1)\).
popBack_ :: (PrimMonad m, VU.Unbox a) => GrowVec (PrimState m) a -> m ()
popBack_ GrowVec {..} = do
  pos <- VGM.read posGV 0
  VGM.write posGV 0 $ max 0 $ pos - 1

-- | \(O(1)\)
length :: (PrimMonad m, VU.Unbox a) => GrowVec (PrimState m) a -> m Int
length GrowVec {posGV} = do
  VGM.read posGV 0

-- | \(O(1)\)
null :: (PrimMonad m, VU.Unbox a) => GrowVec (PrimState m) a -> m Bool
null = fmap (== 0) . length

-- | \(O(n)\)
freeze :: (PrimMonad m, VU.Unbox a) => GrowVec (PrimState m) a -> m (VU.Vector a)
freeze GrowVec {..} = do
  len <- VGM.read posGV 0
  vec <- readMutVar vecGV
  VU.freeze $ VUM.take len vec

-- | \(O(1)\)
unsafeFreeze :: (PrimMonad m, VU.Unbox a) => GrowVec (PrimState m) a -> m (VU.Vector a)
unsafeFreeze GrowVec {..} = do
  len <- VGM.read posGV 0
  vec <- readMutVar vecGV
  VU.unsafeFreeze $ VUM.take len vec
