{-# LANGUAGE RecordWildCards #-}

-- | Immutable Compresed Sparse Row.
module AtCoder.Internal.Csr (Csr(..), build, adj) where

import Data.Foldable (for_)
import Control.Monad.ST (runST)
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Base qualified as VU -- V_2
import Data.Vector.Unboxed.Mutable qualified as VUM

-- | Comperssed Sparse Row.
data Csr e = Csr
  { startCsr :: !(VU.Vector Int),
    elistCsr :: !(VU.Vector e)
  }

-- | \(O(n + m)\)
build :: (VU.Unbox e) => Int -> VU.Vector (Int, e) -> Csr e
build n edges = runST $ do
  start <- VUM.replicate (n + 1) (0 :: Int)

  let (VU.V_2 _ froms _) = edges
  VU.forM_ froms $ \from -> do
    VGM.modify start (+ 1) (from + 1)

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

  startCsr <- VU.unsafeFreeze start
  elistCsr <- VU.unsafeFreeze elist
  return Csr {..}

-- | \(O(1)\)
adj :: (VU.Unbox e) => Csr e -> Int -> VU.Vector e
adj Csr {..} i =
  let il = startCsr VG.! i
      ir = startCsr VG.! (i + 1)
   in VU.slice il (ir - il) elistCsr
