{-# LANGUAGE RecordWildCards #-}

-- | ac-library-hs only.
module AtCoder.Internal.Buffer (Buffer (..), new, pushBack, popBack, length, clear, unsafeFreeze) where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)
import Prelude hiding (length)

-- | Pushable vector with fixed size capacity.
data Buffer s a = Buffer
  { lenB :: !(VUM.MVector s Int),
    vecB :: !(VUM.MVector s a)
  }

-- | \(O(n)\)
new :: (PrimMonad m, VU.Unbox a) => Int -> m (Buffer (PrimState m) a)
new capacity = do
  lenB <- VUM.replicate 1 (0 :: Int)
  vecB <- VUM.unsafeNew capacity
  return Buffer {..}

-- | \(O(1)\)
pushBack :: (HasCallStack, PrimMonad m, VU.Unbox a) => Buffer (PrimState m) a -> a -> m ()
pushBack Buffer {..} e = do
  len <- VGM.read lenB 0
  VGM.write vecB len e
  VGM.write lenB 0 (len + 1)

-- | \(O(1)\)
popBack :: (PrimMonad m, VU.Unbox a) => Buffer (PrimState m) a -> m (Maybe a)
popBack Buffer {..} = do
  len <- VGM.read lenB 0
  if len == 0
    then return Nothing
    else do
      x <- VGM.read vecB (len - 1)
      VGM.write lenB 0 (len - 1)
      return $ Just x

-- | \(O(1)\)
length :: (PrimMonad m, VU.Unbox a) => Buffer (PrimState m) a -> m Int
length Buffer {..} = do
  VGM.read lenB 0

-- | \(O(1)\)
clear :: (PrimMonad m, VU.Unbox a) => Buffer (PrimState m) a -> m ()
clear Buffer {..} = do
  VGM.write lenB 0 0

-- | \(O(1)\)
unsafeFreeze :: (PrimMonad m, VU.Unbox a) => Buffer (PrimState m) a -> m (VU.Vector a)
unsafeFreeze Buffer {..} = do
  len <- VGM.read lenB 0
  VU.unsafeFreeze $ VUM.take len vecB
