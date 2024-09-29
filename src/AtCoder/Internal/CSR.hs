{-# LANGUAGE RecordWildCards #-}

-- | Immutable Compresed Sparse Row.
module AtCoder.Internal.CSR (CSR(..), adj) where

import AtCoder.Internal.Assert
import AtCoder.Internal.GrowVec qualified as ACGV
import AtCoder.Internal.Queue qualified as ACQ
import Data.Foldable (for_)
import Control.Monad.ST (runST)
import Control.Monad.Extra (whenJustM)
import Control.Monad.Fix (fix)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Primitive.MutVar (readMutVar)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Base qualified as VU -- V_2
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)

-- | Immutable Comperssed Sparse Row.
data CSR e = CSR
  { startCSR :: !(VU.Vector Int),
    elistCSR :: !(VU.Vector e)
  }

build :: (VU.Unbox e) => Int -> VU.Vector (Int, e) -> CSR e
build n edges = runST $ do
  start <- VUM.replicate (n + 1) (0 :: Int)

  let (VU.V_2 _ is _) = edges
  VU.forM_ is $ \i -> do
    VGM.modify start (+ 1) (i + 1)

  for_ [1 .. n] $ \i -> do
    prev <- VGM.read start (i - 1)
    VGM.modify start (+ prev) i

  elist <- VUM.unsafeNew (VU.length edges)
  counter <- VUM.unsafeNew n
  VUM.unsafeCopy counter $ VUM.init start
  VU.forM_ edges $ \(!from, !e) -> do
    c <- VGM.read counter from
    VGM.write elist c e
    VGM.write counter from (c + 1)
  VU.unsafeFreeze elist

  startCSR <- VU.unsafeFreeze start
  elistCSR <- VU.unsafeFreeze elist
  return CSR {..}

adj :: (VU.Unbox e) => CSR e -> Int -> VU.Vector e
adj CSR {..} i =
  let il = startCSR VG.! i
      ir = startCSR VG.! (i + 1)
   in VU.slice il (ir - il) elistCSR
