{-# LANGUAGE RecordWildCards #-}

-- | Queue with fixed size capacity.
module AtCoder.Internal.Queue
  ( Queue (..),
    new,
    pushBack,
    popFront,
    capacity,
    length,
    null,
    clear,
    freeze,
    unsafeFreeze,
  )
where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)
import Prelude hiding (length, null)

-- | Queue with fixed size capacity.
data Queue s a = Queue
  { -- | Stores [l, r) range in the `vecQ`.
    posQ :: !(VUM.MVector s Int),
    vecQ :: !(VUM.MVector s a)
  }

-- | \(O(n)\) Creates `Queue` with capacity \(n\).
new :: (PrimMonad m, VU.Unbox a) => Int -> m (Queue (PrimState m) a)
new n = do
  posQ <- VUM.replicate 2 (0 :: Int)
  vecQ <- VUM.unsafeNew n
  pure Queue {..}

-- | \(O(1)\) Appends an element to the back.
pushBack :: (HasCallStack, PrimMonad m, VU.Unbox a) => Queue (PrimState m) a -> a -> m ()
pushBack Queue {..} e = do
  VGM.modifyM
    posQ
    ( \r -> do
        VGM.write vecQ r e
        pure $ r + 1
    )
    1

-- | \(O(1)\) Removes the first element from the queue and returns it, or `Nothing` if it is empty.
popFront :: (PrimMonad m, VU.Unbox a) => Queue (PrimState m) a -> m (Maybe a)
popFront Queue {..} = do
  l <- VGM.read posQ 0
  r <- VGM.read posQ 1
  if l >= r
    then pure Nothing
    else do
      x <- VGM.read vecQ l
      VGM.write posQ 0 (l + 1)
      pure $ Just x

-- | \(O(1)\) Returns the array size.
capacity :: (VU.Unbox a) => Queue s a -> Int
capacity = VUM.length . vecQ

-- | \(O(1)\) Returns the number of elements in the queue.
length :: (PrimMonad m, VU.Unbox a) => Queue (PrimState m) a -> m Int
length Queue {..} = do
  l <- VGM.read posQ 0
  r <- VGM.read posQ 1
  pure $ r - l

-- | \(O(1)\) Returns `True` if the buffer is empty.
null :: (PrimMonad m, VU.Unbox a) => Queue (PrimState m) a -> m Bool
null = fmap (== 0) . length

-- | \(O(1)\) Sets the `length` to zero.
clear :: (PrimMonad m, VU.Unbox a) => Queue (PrimState m) a -> m ()
clear Queue {..} = do
  VGM.set posQ 0

-- | \(O(n)\) Yields an immutable copy of the mutable vector.
freeze :: (PrimMonad m, VU.Unbox a) => Queue (PrimState m) a -> m (VU.Vector a)
freeze Queue {..} = do
  l <- VGM.read posQ 0
  r <- VGM.read posQ 1
  VU.freeze $ VUM.take (r - l) $ VUM.drop l vecQ

-- | \(O(1)\) Unsafely converts a mutable vector to an immutable one without copying. The mutable
-- vector may not be used after this operation.
unsafeFreeze :: (PrimMonad m, VU.Unbox a) => Queue (PrimState m) a -> m (VU.Vector a)
unsafeFreeze Queue {..} = do
  l <- VGM.read posQ 0
  r <- VGM.read posQ 1
  VU.unsafeFreeze $ VUM.take (r - l) $ VUM.drop l vecQ
