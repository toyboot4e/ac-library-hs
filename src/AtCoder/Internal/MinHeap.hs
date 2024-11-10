{-# LANGUAGE RecordWildCards #-}

-- | Minimum binary heap. Mutable and fixed-sized.
--
-- <https://en.wikipedia.org/wiki/Binary_heap>
module AtCoder.Internal.MinHeap (Heap (..), new, capacity, length, null, clear, push, peek, pop) where

import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)
import Prelude hiding (length, null)

-- | Minimum binary heap. Mutable and fixed-sized.
--
-- Indices are zero-based.
--
-- @
--     0
--   1   2
--  3 4 5 6
-- @
--
-- INVARIANT (min heap): child values are bigger than or equal to their parent value.
data Heap s a = Heap
  { -- | Size of the heap.
    sizeBH_ :: !(VUM.MVector s Int),
    -- | Storage.
    dataBH :: !(VUM.MVector s a)
  }

-- | \(O(n)\) Creates `Heap` with capacity \(n\).
new :: (VU.Unbox a, PrimMonad m) => Int -> m (Heap (PrimState m) a)
new n = do
  sizeBH_ <- VUM.replicate 1 0
  dataBH <- VUM.unsafeNew n
  return Heap {..}

-- | \(O(1)\) Returns the number of elements the heap can hold.
capacity :: (VU.Unbox a) => Heap s a -> Int
capacity = VUM.length . dataBH

-- | \(O(1)\) Returns the number of elements in the heap.
length :: (VU.Unbox a, PrimMonad m) => Heap (PrimState m) a -> m Int
length Heap {sizeBH_} = VGM.unsafeRead sizeBH_ 0

-- | \(O(1)\) Returns `True` if the heap is empty.
null :: (VU.Unbox a, PrimMonad m) => Heap (PrimState m) a -> m Bool
null = fmap (== 0) . length

-- | \(O(1)\) Sets the `length` to zero.
clear :: (VU.Unbox a, PrimMonad m) => Heap (PrimState m) a -> m ()
clear Heap {sizeBH_} = VGM.unsafeWrite sizeBH_ 0 0

-- | \(O(\log n)\) Inserts an element to the heap.
push :: (HasCallStack, Ord a, VU.Unbox a, PrimMonad m) => Heap (PrimState m) a -> a -> m ()
push Heap {..} x = do
  i0 <- VGM.unsafeRead sizeBH_ 0
  VGM.write dataBH i0 x
  VGM.unsafeWrite sizeBH_ 0 $ i0 + 1
  let siftUp i = when (i > 0) $ do
        let iParent = (i - 1) `div` 2
        xParent <- VGM.read dataBH iParent
        when (x < xParent) $ do
          VGM.swap dataBH iParent i
          siftUp iParent
  siftUp i0

-- | \(O(1)\) Returns the smallest value in the heap, or `Nothing` if it is empty.
peek :: (VU.Unbox a, PrimMonad m) => Heap (PrimState m) a -> m (Maybe a)
peek heap = do
  isNull <- null heap
  if isNull
    then return Nothing
    else Just <$> VGM.read (dataBH heap) 0

-- | \(O(\log n)\) Removes the last element from the heap and returns it, or `Nothing` if it is
-- empty.
pop :: (HasCallStack, Ord a, VU.Unbox a, PrimMonad m) => Heap (PrimState m) a -> m (Maybe a)
pop heap@Heap {..} = do
  len <- length heap
  if len == 0
    then return Nothing
    else do
      let n = len - 1
      VGM.unsafeWrite sizeBH_ 0 n
      -- copy the last element to the root
      root <- VGM.read dataBH 0
      VGM.swap dataBH 0 n

      -- xl <= xr <= x
      let siftDown i = do
            let il = 2 * i + 1
            let ir = il + 1
            when (il < n) $ do
              x <- VGM.read dataBH i
              xl <- VGM.read dataBH il
              if ir < n
                then do
                  -- IMPORTANT: swap with the smaller child
                  xr <- VGM.read dataBH ir
                  if xl <= xr && xl < x
                    then do
                      VGM.swap dataBH i il
                      siftDown il
                    else when (xr < x) $ do
                      VGM.swap dataBH i ir
                      siftDown ir
                else when (xl < x) $ do
                  VGM.swap dataBH i il
                  siftDown il

      siftDown 0
      return $ Just root
