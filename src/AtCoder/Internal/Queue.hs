{-# LANGUAGE RecordWildCards #-}

-- | Fixed-sized queue. Internally it has \(l, r\) pair of valid element bounds.
--
-- ==== __Example__
-- >>> import AtCoder.Internal.Queue qualified as Q
-- >>> que <- Q.new @_ @Int 3
-- >>> Q.capacity que
-- 3
--
-- >>> Q.pushBack que 0 -- [0  _  _  _]
-- >>> Q.pushBack que 1 -- [0, 1  _  _]
-- >>> Q.pushBack que 2 -- [0, 1, 2  _]
-- >>> Q.length que
-- 3
--
-- >>> Q.popFront que   -- [_  1, 2  _]
-- Just 0
--
-- >>> Q.freeze que     -- [_  1, 2  _]
-- [1,2]
--
-- >>> Q.pushFront que 10   -- [10, 1, 2  _]
-- >>> Q.pushFront que 1000
-- *** Exception: AtCoder.Internal.Queue.pushFront: no empty front space
-- ...
--
-- >>> Q.unsafeFreeze que -- [10, 1, 2  _]
-- [10,1,2]
--
-- >>> Q.clear que      -- [_  _  _  _]
-- >>> Q.pushBack que 0 -- [0  _  _  _]
-- >>> Q.pushBack que 1 -- [0, 1  _  _]
-- >>> Q.pushBack que 2 -- [0, 1, 2  _]
-- >>> Q.freeze que
-- [0,1,2]
--
-- @since 1.0.0.0
module AtCoder.Internal.Queue
  ( -- * Queue
    Queue,

    -- * Constructor
    new,

    -- * Modifying the queue
    pushBack,
    pushFront,
    popFront,
    popFront_,

    -- * Accessors
    capacity,
    length,
    null,

    -- * Clearing
    clear,

    -- * Conversions
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

-- | Fixed-sized queue. Internally it has \([l, r)\) pair of valid element bounds.
--
-- @since 1.0.0.0
data Queue s a = Queue
  { -- | Stores [l, r) range in the `vecQ`.
    posQ :: !(VUM.MVector s Int),
    vecQ :: !(VUM.MVector s a)
  }

-- | \(O(n)\) Creates `Queue` with capacity \(n\).
--
-- @since 1.0.0.0
{-# INLINE new #-}
new :: (PrimMonad m, VU.Unbox a) => Int -> m (Queue (PrimState m) a)
new n = do
  posQ <- VUM.replicate 2 (0 :: Int)
  vecQ <- VUM.unsafeNew n
  pure Queue {..}

-- | \(O(1)\) Appends an element to the back. Will throw an exception if the index is out of range.
--
-- @since 1.0.0.0
{-# INLINE pushBack #-}
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
--
-- @since 1.0.0.0
{-# INLINE pushFront #-}
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
--
-- @since 1.0.0.0
{-# INLINE popFront #-}
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

-- | \(O(1)\) `popFront` with return value discarded.
--
-- @since 1.0.0.0
{-# INLINE popFront_ #-}
popFront_ :: (PrimMonad m, VU.Unbox a) => Queue (PrimState m) a -> m ()
popFront_ que = do
  _ <- popFront que
  pure ()

-- | \(O(1)\) Returns the array size.
--
-- @since 1.0.0.0
{-# INLINE capacity #-}
capacity :: (VU.Unbox a) => Queue s a -> Int
capacity = VUM.length . vecQ

-- | \(O(1)\) Returns the number of elements in the queue.
--
-- @since 1.0.0.0
{-# INLINE length #-}
length :: (PrimMonad m, VU.Unbox a) => Queue (PrimState m) a -> m Int
length Queue {..} = do
  l <- VGM.unsafeRead posQ 0
  r <- VGM.unsafeRead posQ 1
  pure $ r - l

-- | \(O(1)\) Returns `True` if the buffer is empty.
--
-- @since 1.0.0.0
{-# INLINE null #-}
null :: (PrimMonad m, VU.Unbox a) => Queue (PrimState m) a -> m Bool
null = (<$>) (== 0) . length

-- | \(O(1)\) Sets the `length` to zero.
--
-- @since 1.0.0.0
{-# INLINE clear #-}
clear :: (PrimMonad m, VU.Unbox a) => Queue (PrimState m) a -> m ()
clear Queue {..} = do
  VGM.set posQ 0

-- | \(O(n)\) Yields an immutable copy of the mutable vector.
--
-- @since 1.0.0.0
{-# INLINE freeze #-}
freeze :: (PrimMonad m, VU.Unbox a) => Queue (PrimState m) a -> m (VU.Vector a)
freeze Queue {..} = do
  l <- VGM.unsafeRead posQ 0
  r <- VGM.unsafeRead posQ 1
  VU.freeze $ VUM.take (r - l) $ VUM.drop l vecQ

-- | \(O(1)\) Unsafely converts a mutable vector to an immutable one without copying. The mutable
-- vector may not be used after this operation.
--
-- @since 1.0.0.0
{-# INLINE unsafeFreeze #-}
unsafeFreeze :: (PrimMonad m, VU.Unbox a) => Queue (PrimState m) a -> m (VU.Vector a)
unsafeFreeze Queue {..} = do
  l <- VGM.unsafeRead posQ 0
  r <- VGM.unsafeRead posQ 1
  VU.unsafeFreeze $ VUM.take (r - l) $ VUM.drop l vecQ
