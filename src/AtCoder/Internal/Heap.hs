{-# LANGUAGE RecordWildCards #-}

-- | Minimum binary heap. Mutable and fixed-sized.
--
-- <https://en.wikipedia.org/wiki/Binary_heap>
module AtCoder.Internal.Heap (Heap, new, length, null, clear, push, peek, pop) where

import AtCoder.Internal.Assert
import AtCoder.Internal.Buffer qualified as ACB
import AtCoder.Internal.GrowVec qualified as ACGV
import AtCoder.Internal.McfCSR qualified as McfCSR
import AtCoder.Internal.Queue qualified as ACQ
import Control.Monad
import Control.Monad (unless, when)
import Control.Monad.Extra (whenJustM)
import Control.Monad.Fix (fix)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bits
import Data.Coerce
import Data.Foldable (for_)
import Data.Function
import Data.Functor.Identity
import Data.Kind
import Data.Ord
import Data.Primitive.MutVar (readMutVar)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Base qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)
import Prelude hiding (length, null)

-- TODO: write random test

-- | Minimum binary heap. Mutable and fixed-sized.
--
-- Indices are zero-based.
--
-- @
--       0
--    1    2
--  3  4  5 6
-- @
--
-- INVARIANT (min heap): xl <= x < xr.
data Heap s a = Heap
  { -- | Size of the heap.
    sizeBH_ :: !(VUM.MVector s Int),
    -- | Storage.
    dataBH :: !(VUM.MVector s a)
  }

-- | \(O(n)\)
new :: (VU.Unbox a, PrimMonad m) => Int -> m (Heap (PrimState m) a)
new n = do
  sizeBH_ <- VUM.replicate 1 0
  dataBH <- VUM.unsafeNew n
  return Heap {..}

-- | \(O(1)\)
length :: (VU.Unbox a, PrimMonad m) => Heap (PrimState m) a -> m Int
length Heap {sizeBH_} = VGM.unsafeRead sizeBH_ 0

-- | \(O(1)\)
null :: (VU.Unbox a, PrimMonad m) => Heap (PrimState m) a -> m Bool
null = fmap (== 0) . length

-- | \(O(1)\)
clear :: (VU.Unbox a, PrimMonad m) => Heap (PrimState m) a -> m ()
clear Heap {sizeBH_} = VGM.unsafeWrite sizeBH_ 0 0

-- | \(O(\log n)\)
push :: (Ord  a,VU.Unbox a, PrimMonad m) => Heap (PrimState m) a -> a -> m ()
push heap@Heap {..} x = do
  i0 <- length heap
  VGM.write dataBH i0 x
  -- keep up with the invariant
  let siftUp i = do
        let iParent = (i - 1) `div` 2
        xParent <- VGM.read dataBH iParent
        let isCompatible = case (compare x xParent, odd i) of
              -- min heap comparison: leftChild <= parent
              (EQ, True) -> True
              (LT, True) -> True
              (GT, False) -> True
              _ -> False
        unless isCompatible $ do
          VGM.swap dataBH iParent i
          siftUp iParent
  siftUp i0

-- | \(O(1)\)
peek :: (VU.Unbox a, PrimMonad m) => Heap (PrimState m) a -> m (Maybe a)
peek heap = do
  isNull <- null heap
  if isNull
    then return Nothing
    else Just <$> VGM.read (dataBH heap) 0

-- | \(O(\log n)\)
pop :: (Ord a, VU.Unbox a, PrimMonad m) => Heap (PrimState m) a -> m (Maybe a)
pop heap@Heap {..} = do
  len <- length heap
  if len == 0
    then return Nothing
    else do
      -- swap the last element and the root
      x <- VGM.read dataBH (len - 1)
      root <- VGM.exchange dataBH 0 x
      VGM.unsafeWrite sizeBH_ 0 (len - 1)
      -- keep up with the invariant
      let siftDown i = do
            let il = 2 * i + 1
            xl <- VGM.read dataBH il
            xr <- VGM.read dataBH $ il + 1
            unless (xl <= x && x < xr) $ do
              -- min heap: go left
              VGM.swap dataBH i il
              siftDown il
      siftDown 0
      return $ Just root
