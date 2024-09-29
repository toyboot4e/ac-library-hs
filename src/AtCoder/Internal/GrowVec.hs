{-# LANGUAGE RecordWildCards #-}

-- | Growable vector with indirection and runtime overhead.
module AtCoder.Internal.GrowVec (GrowVec (..), new, read, pushBack, length, unsafeFreeze) where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Primitive.MutVar (MutVar, newMutVar, readMutVar, writeMutVar)
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack
import Prelude hiding (length, read)

-- | Growable vector with indirection and runtime overhead.
data GrowVec s a = GrowVec
  { -- | Stores [l, r) range in the `vecGV`.
    posGV :: !(VUM.MVector s Int),
    vecGV :: !(MutVar s (VUM.MVector s a))
  }

-- | \(O(n)\)
new :: (PrimMonad m, VU.Unbox a) => Int -> m (GrowVec (PrimState m) a)
new capacity = do
  posGV <- VUM.replicate 1 (0 :: Int)
  vecGV <- newMutVar =<< VUM.unsafeNew capacity
  return GrowVec {..}

-- | \(O(1)\)
read :: (PrimMonad m, VU.Unbox a) => GrowVec (PrimState m) a -> Int -> m a
read GrowVec {..} i = do
  vec <- readMutVar vecGV
  VGM.read vec i

-- | Amortized \(O(1)\). Grow the capacity twice
pushBack :: (HasCallStack, PrimMonad m, VU.Unbox a) => GrowVec (PrimState m) a -> a -> m ()
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

-- | \(O(1)\)
length :: (PrimMonad m, VU.Unbox a) => GrowVec (PrimState m) a -> m Int
length GrowVec {posGV} = do
  VGM.read posGV 0

-- | \(O(1)\)
unsafeFreeze :: (PrimMonad m, VU.Unbox a) => GrowVec (PrimState m) a -> m (VU.Vector a)
unsafeFreeze GrowVec {..} = do
  len <- VGM.read posGV 0
  vec <- readMutVar vecGV
  VU.unsafeFreeze $ VUM.take len vec
