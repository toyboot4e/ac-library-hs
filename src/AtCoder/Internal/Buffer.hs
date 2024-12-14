{-# LANGUAGE RecordWildCards #-}

-- | Pushable vector with fixed size capacity. Stack. Internally it has the number of elements.
--
-- ==== Example
--
-- >>> import AtCoder.Internal.Buffer qualified as B
-- >>> buf <- B.new @_ @Int 4
-- >>> B.capacity buf
-- 4
-- >>> B.null buf        -- [_   _  _  _]
-- True
-- >>> B.pushBack buf 10 -- [10  _  _ _]
-- >>> B.pushBack buf 11 -- [10, 11  _  _]
-- >>> length buf
-- 2
-- >>> B.read buf 0
-- 10
-- >>> B.write buf 1 0   -- [10, 0,  _  _]
-- >>> B.popBack buf     -- [10  _  _  _]
-- Just 0
-- >>> B.freeze buf
-- [10]
-- >>> B.clear buf       -- []
-- >>> B.null buf
-- True
-- >>> B.unsafeFreeze buf
-- []
--
-- @since 1.0.0
module AtCoder.Internal.Buffer
  ( Buffer (..),
    new,
    build,
    pushBack,
    popBack,
    back,
    read,
    write,
    capacity,
    length,
    null,
    clear,
    freeze,
    unsafeFreeze,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)
import Prelude hiding (length, null, read)

-- | Pushable vector with fixed size capacity. Stack.
--
-- @since 1.0.0
data Buffer s a = Buffer
  { lenB :: !(VUM.MVector s Int),
    vecB :: !(VUM.MVector s a)
  }

-- | \(O(n)\) Creates a buffer with capacity \(n\).
--
-- @since 1.0.0
new :: (PrimMonad m, VU.Unbox a) => Int -> m (Buffer (PrimState m) a)
new n = do
  lenB <- VUM.replicate 1 (0 :: Int)
  vecB <- VUM.unsafeNew n
  pure Buffer {..}

-- | \(O(n)\) Creates a buffer with capacity \(n\) with initial values.
--
-- @since 1.0.0
build :: (PrimMonad m, VU.Unbox a) => VU.Vector a -> m (Buffer (PrimState m) a)
build xs = do
  lenB <- VUM.replicate 1 $ VU.length xs
  vecB <- VU.thaw xs
  pure Buffer {..}

-- | \(O(1)\) Appends an element to the back.
--
-- @since 1.0.0
pushBack :: (HasCallStack, PrimMonad m, VU.Unbox a) => Buffer (PrimState m) a -> a -> m ()
pushBack Buffer {..} e = do
  len <- VGM.read lenB 0
  VGM.write vecB len e
  VGM.write lenB 0 (len + 1)

-- | \(O(1)\) Removes the last element from the buffer and returns it, or `Nothing` if it is empty.
--
-- @since 1.0.0
popBack :: (PrimMonad m, VU.Unbox a) => Buffer (PrimState m) a -> m (Maybe a)
popBack Buffer {..} = do
  len <- VGM.read lenB 0
  if len == 0
    then pure Nothing
    else do
      x <- VGM.read vecB (len - 1)
      VGM.write lenB 0 (len - 1)
      pure $ Just x

-- | \(O(1)\) Returns the last value in the buffer, or `Nothing` if it is empty.
--
-- @since 1.0.0
back :: (PrimMonad m, VU.Unbox a) => Buffer (PrimState m) a -> m (Maybe a)
back Buffer {..} = do
  len <- VGM.read lenB 0
  if len == 0
    then pure Nothing
    else do
      x <- VGM.read vecB (len - 1)
      pure $ Just x

-- | \(O(1)\) Yields the element at the given position. Will throw an exception if the index is out
-- of range.
--
-- @since 1.0.0
read :: (HasCallStack, PrimMonad m, VU.Unbox a) => Buffer (PrimState m) a -> Int -> m a
read Buffer {..} i = do
  len <- VGM.read lenB 0
  let !_ = ACIA.checkIndex "AtCoder.Internal.Buffer.read" i len
  VGM.read vecB i

-- | \(O(1)\) Writes to the element at the given position. Will throw an exception if the index is
-- out of range.
--
-- @since 1.0.0
write :: (HasCallStack, PrimMonad m, VU.Unbox a) => Buffer (PrimState m) a -> Int -> a -> m ()
write Buffer {..} i e = do
  len <- VGM.read lenB 0
  let !_ = ACIA.checkIndex "AtCoder.Internal.Buffer.write" i len
  VGM.write vecB i e

-- | \(O(1)\) Returns the array size.
--
-- @since 1.0.0
capacity :: (VU.Unbox a) => Buffer s a -> Int
capacity = VUM.length . vecB

-- | \(O(1)\) Returns the number of elements in the buffer.
--
-- @since 1.0.0
length :: (PrimMonad m, VU.Unbox a) => Buffer (PrimState m) a -> m Int
length Buffer {..} = do
  VGM.read lenB 0

-- | \(O(1)\) Returns `True` if the buffer is empty.
--
-- @since 1.0.0
null :: (PrimMonad m, VU.Unbox a) => Buffer (PrimState m) a -> m Bool
null = (<$>) (== 0) . length

-- | \(O(1)\) Sets the `length` to zero.
--
-- @since 1.0.0
clear :: (PrimMonad m, VU.Unbox a) => Buffer (PrimState m) a -> m ()
clear Buffer {..} = do
  VGM.write lenB 0 0

-- | \(O(n)\) Yields an immutable copy of the mutable vector.
--
-- @since 1.0.0
freeze :: (PrimMonad m, VU.Unbox a) => Buffer (PrimState m) a -> m (VU.Vector a)
freeze Buffer {..} = do
  len <- VGM.read lenB 0
  VU.freeze $ VUM.take len vecB

-- | \(O(1)\) Unsafely converts a mutable vector to an immutable one without copying. The mutable
-- vector may not be used after this operation.
--
-- @since 1.0.0
unsafeFreeze :: (PrimMonad m, VU.Unbox a) => Buffer (PrimState m) a -> m (VU.Vector a)
unsafeFreeze Buffer {..} = do
  len <- VGM.read lenB 0
  VU.unsafeFreeze $ VUM.take len vecB
