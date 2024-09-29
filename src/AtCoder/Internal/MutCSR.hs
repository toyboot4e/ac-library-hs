{-# LANGUAGE RecordWildCards #-}

-- | Mutable Compresed Sparse Row. Weights can be modified.
module AtCoder.Internal.MutCSR (CSR (..), build, adj, iAdj) where

-- V_2
import AtCoder.Internal.Assert
import AtCoder.Internal.GrowVec qualified as ACGV
import AtCoder.Internal.Queue qualified as ACQ
import Control.Monad (unless, when)
import Control.Monad.Extra (whenJustM)
import Control.Monad.Fix (fix)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Foldable (for_)
import Data.Primitive.MutVar (readMutVar)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Base qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)

-- | Mutable Comperssed Sparse Row. Weights can be modified.
data CSR s e w = CSR
  { startCSR :: !(VU.Vector Int),
    elistCSR :: !(VU.Vector e),
    wCSR :: !(VUM.MVector s w)
  }

build :: (VU.Unbox e, VU.Unbox w, PrimMonad m) => Int -> VU.Vector (Int, e, w) -> m (CSR (PrimState m) e w)
build n edges = do
  start <- VUM.replicate (n + 1) (0 :: Int)

  let (VU.V_3 _ is _ _) = edges
  VU.forM_ is $ \i -> do
    VGM.modify start (+ 1) (i + 1)

  for_ [1 .. n] $ \i -> do
    prev <- VGM.read start (i - 1)
    VGM.modify start (+ prev) i

  elist <- VUM.unsafeNew (VU.length edges)
  wCSR <- VUM.unsafeNew (VU.length edges)
  counter <- VUM.unsafeNew n
  VUM.unsafeCopy counter $ VUM.init start
  VU.forM_ edges $ \(!from, !e, !w) -> do
    c <- VGM.read counter from
    VGM.write elist c e
    VGM.write wCSR c w
    VGM.write counter from (c + 1)

  startCSR <- VU.unsafeFreeze start
  elistCSR <- VU.unsafeFreeze elist
  return CSR {..}

adj :: (VU.Unbox e) => CSR s e w -> Int -> VU.Vector e
adj CSR {..} i =
  let il = startCSR VG.! i
      ir = startCSR VG.! (i + 1)
   in VU.slice il (ir - il) elistCSR

iAdj :: (VU.Unbox e) => CSR s e w -> Int -> VU.Vector (Int, e)
iAdj CSR {..} i =
  let il = startCSR VG.! i
      ir = startCSR VG.! (i + 1)
      es = VU.slice il (ir - il) elistCSR
   in VU.imap ((,) . (+ il)) es
