{-# LANGUAGE RecordWildCards #-}

-- | Growable vector with some runtime overhead (by `MutVar`).
--
-- ==== Example
-- >>> import AtCoder.Internal.GrowVec qualified as GV
-- >>> growVec <- GV.new @_ @Int 0
-- >>> GV.null growVec
-- True
--
-- >>> GV.pushBack growVec 10
-- >>> GV.pushBack growVec 11
-- >>> GV.pushBack growVec 12
-- >>> GV.freeze growVec
-- [10,11,12]
--
-- >>> GV.length growVec
-- 3
--
-- >>> GV.capacity growVec
-- 4
--
-- >>> GV.write growVec 1 20
-- >>> GV.read growVec 1
-- 20
--
-- >>> GV.popBack growVec
-- Just 12
--
-- >>> GV.popBack growVec
-- Just 20
--
-- >>> GV.reserve growVec 20
-- >>> GV.capacity growVec
-- 20
--
-- >>> GV.unsafeFreeze growVec
-- [10]
--
-- @since 1.0.0.0
module AtCoder.Internal.GrowVec
  ( -- * Growable vector
    GrowVec (vecGV),

    -- * Constructions

    -- ** Initialization
    new,
    build,

    -- ** Reserving
    reserve,

    -- * Accessing individual elements
    read,
    write,

    -- * Modifying the buffer
    pushBack,
    popBack,
    popBack_,

    -- * Accessors
    length,
    capacity,
    null,

    -- * Conversion
    freeze,
    unsafeFreeze,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Primitive.MutVar (MutVar, newMutVar, readMutVar, writeMutVar)
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)
import Prelude hiding (length, null, read)

-- | Growable vector with some runtime overhead.
--
-- @since 1.0.0.0
data GrowVec s a = GrowVec
  { -- | Stores [l, r) range in the `vecGV`.
    posGV :: !(VUM.MVector s Int),
    -- | @since 1.0.0.0
    vecGV :: !(MutVar s (VUM.MVector s a))
  }

-- | \(O(n)\) Creates `GrowVec` with initial capacity \(n\).
--
-- @since 1.0.0.0
{-# INLINE new #-}
new :: (PrimMonad m, VU.Unbox a) => Int -> m (GrowVec (PrimState m) a)
new n = do
  posGV <- VUM.replicate 1 (0 :: Int)
  vecGV <- newMutVar =<< VUM.unsafeNew n
  pure GrowVec {..}

-- | \(O(n)\) Creates `GrowVec` with initial values.
--
-- @since 1.0.0.0
{-# INLINE build #-}
build :: (PrimMonad m, VU.Unbox a) => VU.Vector a -> m (GrowVec (PrimState m) a)
build xs = do
  posGV <- VUM.replicate 1 $ VU.length xs
  vecGV <- newMutVar =<< VU.thaw xs
  pure GrowVec {..}

-- | \(O(n)\) Reserves the internal storage capacity.
--
-- @since 1.0.0.0
{-# INLINE reserve #-}
reserve :: (HasCallStack, PrimMonad m, VU.Unbox a) => GrowVec (PrimState m) a -> Int -> m ()
reserve GrowVec {..} len = do
  vec <- readMutVar vecGV
  when (VUM.length vec < len) $ do
    newVec <- VUM.unsafeGrow vec (len - VUM.length vec)
    writeMutVar vecGV newVec

-- | \(O(1)\) Yields the element at the given position. Will throw an exception if the index is out
-- of range.
--
-- @since 1.0.0.0
{-# INLINE read #-}
read :: (HasCallStack, PrimMonad m, VU.Unbox a) => GrowVec (PrimState m) a -> Int -> m a
read GrowVec {..} i = do
  vec <- readMutVar vecGV
  let len = VUM.length vec
  let !_ = ACIA.checkIndex "AtCoder.Internal.GrowVec.read" i len
  VGM.read vec i

-- | \(O(1)\) Writes to the element at the given position. Will throw an exception if the index is
-- out of range.
--
-- @since 1.0.0.0
{-# INLINE write #-}
write :: (HasCallStack, PrimMonad m, VU.Unbox a) => GrowVec (PrimState m) a -> Int -> a -> m ()
write GrowVec {..} i x = do
  vec <- readMutVar vecGV
  let len = VUM.length vec
  let !_ = ACIA.checkIndex "AtCoder.Internal.GrowVec.write" i len
  VGM.write vec i x

-- | Amortized \(O(1)\). Grow the capacity twice
--
-- @since 1.0.0.0
{-# INLINE pushBack #-}
pushBack :: (PrimMonad m, VU.Unbox a) => GrowVec (PrimState m) a -> a -> m ()
pushBack GrowVec {..} e = do
  len <- VGM.unsafeRead posGV 0
  vec <- do
    vec <- readMutVar vecGV
    if VUM.length vec > len
      then pure vec
      else do
        -- double the internal vector length
        newVec <- VUM.unsafeGrow vec $ max 1 len
        writeMutVar vecGV newVec
        pure newVec

  VGM.unsafeModifyM
    posGV
    ( \r -> do
        VGM.write vec r e
        pure $ r + 1
    )
    0

-- | \(O(1)\) Removes the last element from the buffer and returns it, or `Nothing` if it is empty.
--
-- @since 1.0.0.0
{-# INLINE popBack #-}
popBack :: (PrimMonad m, VU.Unbox a) => GrowVec (PrimState m) a -> m (Maybe a)
popBack GrowVec {..} = do
  pos <- VGM.unsafeRead posGV 0
  if pos <= 0
    then pure Nothing
    else do
      VGM.unsafeWrite posGV 0 $ pos - 1
      vec <- readMutVar vecGV
      Just <$> VGM.read vec (pos - 1)

-- | \(O(1)\) `popBack` with return value discarded.
--
-- @since 1.0.0.0
{-# INLINE popBack_ #-}
popBack_ :: (PrimMonad m, VU.Unbox a) => GrowVec (PrimState m) a -> m ()
popBack_ GrowVec {..} = do
  pos <- VGM.unsafeRead posGV 0
  VGM.unsafeWrite posGV 0 $ max 0 $ pos - 1

-- | \(O(1)\) Returns the number of elements in the vector.
--
-- @since 1.0.0.0
{-# INLINE length #-}
length :: (PrimMonad m, VU.Unbox a) => GrowVec (PrimState m) a -> m Int
length GrowVec {posGV} = do
  VGM.unsafeRead posGV 0

-- | \(O(1)\) Returns the capacity of the internal the vector.
--
-- @since 1.0.0.0
{-# INLINE capacity #-}
capacity :: (PrimMonad m, VU.Unbox a) => GrowVec (PrimState m) a -> m Int
capacity GrowVec {vecGV} = do
  vec <- readMutVar vecGV
  pure $ VUM.length vec

-- | \(O(1)\) Returns `True` if the vector is empty.
--
-- @since 1.0.0.0
{-# INLINE null #-}
null :: (PrimMonad m, VU.Unbox a) => GrowVec (PrimState m) a -> m Bool
null = (<$>) (== 0) . length

-- | \(O(n)\) Yields an immutable copy of the mutable vector.
--
-- @since 1.0.0.0
{-# INLINE freeze #-}
freeze :: (PrimMonad m, VU.Unbox a) => GrowVec (PrimState m) a -> m (VU.Vector a)
freeze GrowVec {..} = do
  len <- VGM.unsafeRead posGV 0
  vec <- readMutVar vecGV
  VU.freeze $ VUM.take len vec

-- | \(O(1)\) Unsafely converts a mutable vector to an immutable one without copying. The mutable
-- vector may not be used after this operation.
--
-- @since 1.0.0.0
{-# INLINE unsafeFreeze #-}
unsafeFreeze :: (PrimMonad m, VU.Unbox a) => GrowVec (PrimState m) a -> m (VU.Vector a)
unsafeFreeze GrowVec {..} = do
  len <- VGM.unsafeRead posGV 0
  vec <- readMutVar vecGV
  VU.unsafeFreeze $ VUM.take len vec
