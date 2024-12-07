{-# LANGUAGE RecordWildCards #-}

-- | Queue with fixed size capacity.
--
-- >>> que <- new @_ @Int 3
-- >>> capacity que
-- 3
-- >>> pushBack que 0
-- >>> pushBack que 1
-- >>> pushBack que 2
-- >>> length que
-- 3
-- >>> popFront que
-- Just 0
-- >>> freeze que
-- [1,2]
-- >>> pushFront que 10
-- >>> pushFront que 1000
-- *** Exception: AtCoder.Internal.Queue.pushFront: no empty front space
-- ...
-- >>> unsafeFreeze que
-- [10,1,2]
-- >>> clear que
-- >>> pushBack que 0
-- >>> pushBack que 1
-- >>> pushBack que 2
-- >>> freeze que
-- [0,1,2]
module AtCoder.Internal.Queue
  ( Queue (..),
    new,
    pushBack,
    pushFront,
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

-- | \(O(1)\) Appends an element to the back. Will throw an exception if the index is out of range.
pushBack :: (HasCallStack, PrimMonad m, VU.Unbox a) => Queue (PrimState m) a -> a -> m ()
pushBack Queue {..} e = do
  VGM.unsafeModifyM
    posQ
    ( \r -> do
        VGM.write vecQ r e
        pure $ r + 1
    )
    1

-- | \(O(1)\) Appends an element to the back. Will throw an exception if the index is out of range.
pushFront :: (HasCallStack, PrimMonad m, VU.Unbox a) => Queue (PrimState m) a -> a -> m ()
pushFront Queue {..} e = do
  l0 <- VGM.unsafeRead posQ 0
  if l0 == 0
    then error "AtCoder.Internal.Queue.pushFront: no empty front space"
    else do
      VGM.unsafeModifyM
        posQ
        ( \l -> do
            VGM.write vecQ (l - 1) e
            pure $ l - 1
        )
        0

-- | \(O(1)\) Removes the first element from the queue and returns it, or `Nothing` if it is empty.
popFront :: (PrimMonad m, VU.Unbox a) => Queue (PrimState m) a -> m (Maybe a)
popFront Queue {..} = do
  l <- VGM.unsafeRead posQ 0
  r <- VGM.unsafeRead posQ 1
  if l >= r
    then pure Nothing
    else do
      x <- VGM.read vecQ l
      VGM.unsafeWrite posQ 0 (l + 1)
      pure $ Just x

-- | \(O(1)\) Returns the array size.
capacity :: (VU.Unbox a) => Queue s a -> Int
capacity = VUM.length . vecQ

-- | \(O(1)\) Returns the number of elements in the queue.
length :: (PrimMonad m, VU.Unbox a) => Queue (PrimState m) a -> m Int
length Queue {..} = do
  l <- VGM.unsafeRead posQ 0
  r <- VGM.unsafeRead posQ 1
  pure $ r - l

-- | \(O(1)\) Returns `True` if the buffer is empty.
null :: (PrimMonad m, VU.Unbox a) => Queue (PrimState m) a -> m Bool
null = (<$>) (== 0) . length

-- | \(O(1)\) Sets the `length` to zero.
clear :: (PrimMonad m, VU.Unbox a) => Queue (PrimState m) a -> m ()
clear Queue {..} = do
  VGM.set posQ 0

-- | \(O(n)\) Yields an immutable copy of the mutable vector.
freeze :: (PrimMonad m, VU.Unbox a) => Queue (PrimState m) a -> m (VU.Vector a)
freeze Queue {..} = do
  l <- VGM.unsafeRead posQ 0
  r <- VGM.unsafeRead posQ 1
  VU.freeze $ VUM.take (r - l) $ VUM.drop l vecQ

-- | \(O(1)\) Unsafely converts a mutable vector to an immutable one without copying. The mutable
-- vector may not be used after this operation.
unsafeFreeze :: (PrimMonad m, VU.Unbox a) => Queue (PrimState m) a -> m (VU.Vector a)
unsafeFreeze Queue {..} = do
  l <- VGM.unsafeRead posQ 0
  r <- VGM.unsafeRead posQ 1
  VU.unsafeFreeze $ VUM.take (r - l) $ VUM.drop l vecQ
