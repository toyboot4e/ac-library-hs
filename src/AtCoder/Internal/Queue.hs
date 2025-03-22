{-# LANGUAGE RecordWildCards #-}

-- | Fixed-sized queue. Internally it has an \([l, r)\) pair of valid element bounds.
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
-- >>> Q.peekBack que
-- Nothing
--
-- >>> Q.pushBack que 0 -- [0  _  _  _]
-- >>> Q.peekBack que
-- Just 0
--
-- >>> Q.pushBack que 1 -- [0, 1  _  _]
-- >>> Q.pushBack que 2 -- [0, 1, 2  _]
-- >>> Q.popBack que    -- [0, 1  _  _]
-- Just 2
--
-- >>> Q.freeze que
-- [0,1]
--
-- @since 1.0.0.0
module AtCoder.Internal.Queue
  ( -- * Queue
    Queue,

    -- * Constructor
    new,

    -- * Metadata
    capacity,
    length,
    null,

    -- * Modifications

    -- ** Peek
    peekBack,
    peekFront,

    -- ** Push
    pushBack,
    pushFront,

    -- ** Pop
    popBack,
    popBack_,
    popFront,
    popFront_,

    -- ** Clear (reset)
    clear,

    -- * Conversions
    freeze,
    unsafeFreeze,
  )
where

import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Control.Monad.ST (ST)
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)
import Prelude hiding (length, null)

-- | Fixed-sized queue. Internally it has an \([l, r)\) pair of valid element bounds.
--
-- @since 1.0.0.0
data Queue s a = Queue
  { -- | Stores [l, r) range in the `vecQ`.
    posQ :: !(VUM.MVector s Int),
    vecQ :: !(VUM.MVector s a)
  }

-- | \(O(n)\) Creates a `Queue` with capacity \(n\).
--
-- @since 1.0.0.0
{-# INLINE new #-}
new :: (PrimMonad m, VU.Unbox a) => Int -> m (Queue (PrimState m) a)
new n = stToPrim $ newST n

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
length que = stToPrim $ lengthST que

-- | \(O(1)\) Returns `True` if the buffer is empty.
--
-- @since 1.0.0.0
{-# INLINE null #-}
null :: (PrimMonad m, VU.Unbox a) => Queue (PrimState m) a -> m Bool
null = (<$>) (== 0) . length

-- | \(O(1)\) Peeks the last element in the queue.
--
-- @since 1.2.3.0
{-# INLINE peekBack #-}
peekBack :: (PrimMonad m, VU.Unbox a) => Queue (PrimState m) a -> m (Maybe a)
peekBack que = stToPrim $ peekBackST que

-- | \(O(1)\) Peeks the first element in the queue.
--
-- @since 1.2.3.0
{-# INLINE peekFront #-}
peekFront :: (PrimMonad m, VU.Unbox a) => Queue (PrimState m) a -> m (Maybe a)
peekFront que = stToPrim $ peekFrontST que

-- | \(O(1)\) Appends an element to the back. Will throw an exception if the index is out of range.
--
-- @since 1.0.0.0
{-# INLINE pushBack #-}
pushBack :: (HasCallStack, PrimMonad m, VU.Unbox a) => Queue (PrimState m) a -> a -> m ()
pushBack que e = stToPrim $ pushBackST que e

-- | \(O(1)\) Appends an element to the back. Will throw an exception if the index is out of range.
--
-- @since 1.0.0.0
{-# INLINE pushFront #-}
pushFront :: (HasCallStack, PrimMonad m, VU.Unbox a) => Queue (PrimState m) a -> a -> m ()
pushFront que e = stToPrim $ pushFrontST que e

-- | \(O(1)\) Removes the last element from the queue and returns it, or `Nothing` if it is empty.
--
-- @since 1.2.3.0
{-# INLINE popBack #-}
popBack :: (PrimMonad m, VU.Unbox a) => Queue (PrimState m) a -> m (Maybe a)
popBack que = stToPrim $ popBackST que

-- | \(O(1)\) `popBack` with the return value discarded.
--
-- @since 1.2.3.0
{-# INLINE popBack_ #-}
popBack_ :: (PrimMonad m, VU.Unbox a) => Queue (PrimState m) a -> m ()
popBack_ que = stToPrim $ popBackST_ que

-- | \(O(1)\) Removes the first element from the queue and returns it, or `Nothing` if it is empty.
--
-- @since 1.0.0.0
{-# INLINE popFront #-}
popFront :: (PrimMonad m, VU.Unbox a) => Queue (PrimState m) a -> m (Maybe a)
popFront que = stToPrim $ popFrontST que

-- | \(O(1)\) `popFront` with the return value discarded.
--
-- @since 1.0.0.0
{-# INLINE popFront_ #-}
popFront_ :: (PrimMonad m, VU.Unbox a) => Queue (PrimState m) a -> m ()
popFront_ que = stToPrim $ popFrontST_ que

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
freeze que = stToPrim $ freezeST que

-- | \(O(1)\) Unsafely converts a mutable vector to an immutable one without copying. The mutable
-- vector may not be used after this operation.
--
-- @since 1.0.0.0
{-# INLINE unsafeFreeze #-}
unsafeFreeze :: (PrimMonad m, VU.Unbox a) => Queue (PrimState m) a -> m (VU.Vector a)
unsafeFreeze que = stToPrim $ unsafeFreezeST que

-- -------------------------------------------------------------------------------------------------
-- Internal
-- -------------------------------------------------------------------------------------------------

{-# INLINEABLE newST #-}
newST :: (VU.Unbox a) => Int -> ST s (Queue s a)
newST n = do
  posQ <- VUM.replicate 2 (0 :: Int)
  vecQ <- VUM.unsafeNew n
  pure Queue {..}

{-# INLINEABLE lengthST #-}
lengthST :: (VU.Unbox a) => Queue s a -> ST s Int
lengthST Queue {..} = do
  l <- VGM.unsafeRead posQ 0
  r <- VGM.unsafeRead posQ 1
  pure $ r - l

{-# INLINEABLE peekBackST #-}
peekBackST :: (VU.Unbox a) => Queue s a -> ST s (Maybe a)
peekBackST Queue {..} = do
  l <- VGM.unsafeRead posQ 0
  r <- VGM.unsafeRead posQ 1
  if l >= r
    then pure Nothing
    else Just <$> VGM.read vecQ (r - 1)

{-# INLINEABLE peekFrontST #-}
peekFrontST :: (VU.Unbox a) => Queue s a -> ST s (Maybe a)
peekFrontST Queue {..} = do
  l <- VGM.unsafeRead posQ 0
  r <- VGM.unsafeRead posQ 1
  if l >= r
    then pure Nothing
    else Just <$> VGM.read vecQ l

{-# INLINEABLE pushBackST #-}
pushBackST :: (HasCallStack, VU.Unbox a) => Queue s a -> a -> ST s ()
pushBackST Queue {..} e = do
  VGM.unsafeModifyM
    posQ
    ( \r -> do
        VGM.write vecQ r e
        pure $ r + 1
    )
    1

{-# INLINEABLE pushFrontST #-}
pushFrontST :: (HasCallStack, VU.Unbox a) => Queue s a -> a -> ST s ()
pushFrontST Queue {..} e = do
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

{-# INLINEABLE popBackST #-}
popBackST :: (VU.Unbox a) => Queue s a -> ST s (Maybe a)
popBackST Queue {..} = do
  l <- VGM.unsafeRead posQ 0
  r <- VGM.unsafeRead posQ 1
  if l >= r
    then pure Nothing
    else do
      x <- VGM.read vecQ (r - 1)
      VGM.unsafeWrite posQ 1 (r - 1)
      pure $ Just x

{-# INLINEABLE popBackST_ #-}
popBackST_ :: (VU.Unbox a) => Queue s a -> ST s ()
popBackST_ que = do
  _ <- popBackST que
  pure ()

{-# INLINEABLE popFrontST #-}
popFrontST :: (VU.Unbox a) => Queue s a -> ST s (Maybe a)
popFrontST Queue {..} = do
  l <- VGM.unsafeRead posQ 0
  r <- VGM.unsafeRead posQ 1
  if l >= r
    then pure Nothing
    else do
      x <- VGM.read vecQ l
      VGM.unsafeWrite posQ 0 (l + 1)
      pure $ Just x

{-# INLINEABLE popFrontST_ #-}
popFrontST_ :: (VU.Unbox a) => Queue s a -> ST s ()
popFrontST_ que = do
  _ <- popFrontST que
  pure ()

{-# INLINEABLE freezeST #-}
freezeST :: (VU.Unbox a) => Queue s a -> ST s (VU.Vector a)
freezeST Queue {..} = do
  l <- VGM.unsafeRead posQ 0
  r <- VGM.unsafeRead posQ 1
  VU.freeze $ VUM.take (r - l) $ VUM.drop l vecQ

{-# INLINEABLE unsafeFreezeST #-}
unsafeFreezeST :: (VU.Unbox a) => Queue s a -> ST s (VU.Vector a)
unsafeFreezeST Queue {..} = do
  l <- VGM.unsafeRead posQ 0
  r <- VGM.unsafeRead posQ 1
  VU.unsafeFreeze $ VUM.take (r - l) $ VUM.drop l vecQ
