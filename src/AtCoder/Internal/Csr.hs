{-# LANGUAGE RecordWildCards #-}

-- | Immutable Compresed Sparse Row.
--
-- ==== __Example__
-- Create a `Csr` without edge weights using `build`:
--
-- >>> import AtCoder.Internal.Csr qualified as C
-- >>> let csr = build' 3 $ VU.fromList @(Int, Int) [(0, 1), (0, 2), (0, 3), (1, 2), (2, 3)]
-- >>> csr `C.adj` 0
-- [1,2,3]
--
-- >>> csr `C.adj` 1
-- [2]
--
-- >>> csr `C.adj` 2
-- [3]
--
-- Create a `Csr` with edge weights with `build` and retrieve edge with `edgeW`:
--
-- >>> import AtCoder.Internal.Csr qualified as C
-- >>> let csr = build 3 $ VU.fromList @(Int, Int, Int) [(0, 1, 101), (0, 2, 102), (0, 3, 103), (1, 2, 112), (2, 3, 123)]
-- >>> csr `C.adjW` 0
-- [(1,101),(2,102),(3,103)]
--
-- >>> csr `C.adjW` 1
-- [(2,112)]
--
-- >>> csr `C.adjW` 2
-- [(3,123)]
--
-- @since 1.0.0
module AtCoder.Internal.Csr
  ( -- * Compressed sparse row
    Csr,
    -- * Constructor
    build,
    build',
    -- * Accessors
    adj,
    adjW,
    eAdj,
  )
where

import Control.Monad.ST (runST)
import Data.Foldable (for_)
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)

-- | Comperssed Sparse Row representation of a graph.
--
-- @since 1.0.0
data Csr w = Csr
  { startCsr :: !(VU.Vector Int),
    adjCsr :: !(VU.Vector Int),
    wCsr :: !(VU.Vector w)
  }
  deriving
    ( -- | @since 1.0.0
      Eq,
      -- | @since 1.0.0
      Show
    )

-- | \(O(n + m)\) Creates `Csr`.
--
-- @since 1.0.0
{-# INLINE build #-}
build :: (HasCallStack, VU.Unbox w) => Int -> VU.Vector (Int, Int, w) -> Csr w
build n edges = runST $ do
  start <- VUM.replicate (n + 1) (0 :: Int)

  let (!froms, !_, !_) = VU.unzip3 edges
  VU.forM_ froms $ \from -> do
    VGM.modify start (+ 1) (from + 1)

  for_ [1 .. n] $ \i -> do
    prev <- VGM.read start (i - 1)
    VGM.modify start (+ prev) i

  edgeAdj <- VUM.unsafeNew (VU.length edges)
  edgeW <- VUM.unsafeNew (VU.length edges)
  counter <- VUM.unsafeNew n
  VUM.unsafeCopy counter $ VUM.init start
  VU.forM_ edges $ \(!from, !to, !w) -> do
    c <- VGM.read counter from
    VGM.write edgeAdj c to
    VGM.write edgeW c w
    VGM.write counter from (c + 1)

  startCsr <- VU.unsafeFreeze start
  adjCsr <- VU.unsafeFreeze edgeAdj
  wCsr <- VU.unsafeFreeze edgeW
  pure Csr {..}

-- | \(O(n + m)\) Creates `Csr` with no weight.
--
-- @since 1.0.0
{-# INLINE build' #-}
build' :: (HasCallStack) => Int -> VU.Vector (Int, Int) -> Csr ()
build' n edges = build n $ VU.zip3 us vs (VU.replicate (VU.length us) ())
  where
    (!us, !vs) = VU.unzip edges

-- | \(O(1)\) Returns adjacent vertices.
--
-- @since 1.0.0
{-# INLINE adj #-}
adj :: (HasCallStack, VU.Unbox w) => Csr w -> Int -> VU.Vector Int
adj Csr {..} i =
  let il = startCsr VG.! i
      ir = startCsr VG.! (i + 1)
   in VU.slice il (ir - il) adjCsr

-- | \(O(1)\) Returns adjacent vertices with weights.
--
-- @since 1.0.0
{-# INLINE adjW #-}
adjW :: (HasCallStack, VU.Unbox w) => Csr w -> Int -> VU.Vector (Int, w)
adjW Csr {..} i =
  let il = startCsr VG.! i
      ir = startCsr VG.! (i + 1)
   in VU.zip (VU.slice il (ir - il) adjCsr) (VU.slice il (ir - il) wCsr)

-- | \(O(1)\) Returns a vector of @(edgeId, adjacentVertex)@.
--
-- @since 1.0.0
{-# INLINE eAdj #-}
eAdj :: (HasCallStack, VU.Unbox w) => Csr w -> Int -> VU.Vector (Int, Int)
eAdj Csr {..} i =
  let il = startCsr VG.! i
      ir = startCsr VG.! (i + 1)
   in VU.imap ((,) . (+ il)) $ VU.slice il (ir - il) adjCsr
