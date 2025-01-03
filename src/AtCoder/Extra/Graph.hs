-- | Generic graph serach functions. Graphs are abstracted as the number of vertices and a function
-- that returns the neighbors of a vertex.
--
-- Also the `Csr` module is re-exported and it's considered as the primary concrete graph
-- implementation.
--
-- @since 1.1.0.0
module AtCoder.Extra.Graph
  ( module Csr,

    -- * CSR helpers
    swapDupe,
    swapDupe',

    -- * Graph search
    topSort,
  )
where

import AtCoder.Extra.IntSet qualified as IS
import AtCoder.Internal.Buffer qualified as B
import AtCoder.Internal.Csr as Csr
import Control.Monad (when)
import Control.Monad.ST (runST)
import Data.Foldable (for_)
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

-- | \(O(n)\) Converts non-directed edges into directional edges. This is a convenient function for
-- making an input of `build`.
--
-- ==== __Example__
-- Create a non-directed graph:
--
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let gr = Gr.build 3 . Gr.swapDupe $ VU.fromList [(0, 1, ()), (1, 2, ())]
-- >>> gr `Gr.adj` 0
-- [1]
--
-- >>> gr `Gr.adj` 1
-- [0,2]
--
-- >>> gr `Gr.adj` 2
-- [1]
--
-- @since 1.1.0.0
{-# INLINE swapDupe #-}
swapDupe :: (VU.Unbox (Int, Int, w)) => VU.Vector (Int, Int, w) -> VU.Vector (Int, Int, w)
swapDupe = VU.concatMap (\(!u, !v, !w) -> VU.fromListN 2 [(u, v, w), (v, u, w)])

-- | \(O(n)\) Converts non-directed edges into directional edges. This is a convenient function for
-- making an input of `build'`.
--
-- ==== __Example__
-- Create a non-directed graph:
--
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let gr = Gr.build' 3 . Gr.swapDupe' $ VU.fromList [(0, 1), (1, 2)]
-- >>> gr `Gr.adj` 0
-- [1]
--
-- >>> gr `Gr.adj` 1
-- [0,2]
--
-- >>> gr `Gr.adj` 2
-- [1]
--
-- @since 1.1.0.0
{-# INLINE swapDupe' #-}
swapDupe' :: (VU.Unbox (Int, Int)) => VU.Vector (Int, Int) -> VU.Vector (Int, Int)
swapDupe' = VU.concatMap (\(!u, !v) -> VU.fromListN 2 [(u, v), (v, u)])

-- | \(O(n \log n + m)\) Returns the lexicographically smallest topological ordering of given graph.
--
-- ==== Constraints
-- - The graph must be a DAG.
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let n = 5
-- >>> let gr = Gr.build' n $ VU.fromList [(1, 2), (4, 0), (0, 3)]
-- >>> Gr.topSort n (gr `Gr.adj`)
-- [1,2,4,0,3]
--
-- @since 1.1.0.0
{-# INLINE topSort #-}
topSort :: Int -> (Int -> VU.Vector Int) -> VU.Vector Int
topSort n gr = runST $ do
  inDeg <- VUM.replicate n (0 :: Int)
  for_ [0 .. n - 1] $ \u -> do
    VU.forM_ (gr u) $ \v -> do
      VGM.modify inDeg (+ 1) v

  -- start from the vertices with zero in-degrees:
  que <- IS.new n
  inDeg' <- VU.unsafeFreeze inDeg
  VU.iforM_ inDeg' $ \v d -> do
    when (d == 0) $ do
      IS.insert que v

  buf <- B.new n
  let run = do
        IS.deleteMin que >>= \case
          Nothing -> pure ()
          Just u -> do
            B.pushBack buf u
            VU.forM_ (gr u) $ \v -> do
              nv <- subtract 1 <$> VGM.read inDeg v
              VGM.write inDeg v nv
              when (nv == 0) $ do
                IS.insert que v
            run

  run
  B.unsafeFreeze buf
