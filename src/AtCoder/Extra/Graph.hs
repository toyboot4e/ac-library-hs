{-# LANGUAGE LambdaCase #-}

-- | Re-export of the @Csr@ module and generic graph search functions.
--
-- @since 1.1.0.0
module AtCoder.Extra.Graph
  ( -- * Re-export of CSR

    -- | The `Csr.Csr` data type and all the functions such as `build` or `adj` are re-exported.
    module Csr,

    -- * CSR helpers
    swapDupe,
    swapDupe',
    scc,

    -- * Graph search
    topSort,
    blockCut,
    blockCutComponents,
  )
where

import AtCoder.Extra.IntSet qualified as IS
import AtCoder.Internal.Buffer qualified as B
import AtCoder.Internal.Csr as Csr
import AtCoder.Internal.Scc qualified as ACISCC
import Control.Monad (when)
import Control.Monad.ST (runST)
import Data.Bit (Bit (..))
import Data.Foldable (for_)
import Data.Maybe (fromJust)
import Data.Vector qualified as V
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

-- | \(O(n)\) Converts non-directed edges into directional edges. This is a convenient function for
-- making an input to `build`.
--
-- ==== __Example__
-- `swapDupe` duplicates each edge reversing the direction:
--
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> Gr.swapDupe $ VU.fromList [(0, 1, ()), (1, 2, ())]
-- [(0,1,()),(1,0,()),(1,2,()),(2,1,())]
--
-- Create a non-directed graph:
--
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
swapDupe :: (VU.Unbox w) => VU.Vector (Int, Int, w) -> VU.Vector (Int, Int, w)
swapDupe = VU.concatMap (\(!u, !v, !w) -> VU.fromListN 2 [(u, v, w), (v, u, w)])

-- | \(O(n)\) Converts non-directed edges into directional edges. This is a convenient function for
-- making an input to `build'`.
--
-- ==== __Example__
-- `swapDupe'` duplicates each edge reversing the direction:
--
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> Gr.swapDupe' $ VU.fromList [(0, 1), (1, 2)]
-- [(0,1),(1,0),(1,2),(2,1)]
--
-- Create a non-directed graph:
--
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
swapDupe' :: VU.Vector (Int, Int) -> VU.Vector (Int, Int)
swapDupe' = VU.concatMap (\(!u, !v) -> VU.fromListN 2 [(u, v), (v, u)])

-- | \(O(n + m)\) Returns the strongly connected components.
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> -- 0 == 1 -> 2    3
-- >>> let gr = Gr.build' 4 $ VU.fromList [(0, 1), (1, 0), (1, 2)]
-- >>> Gr.scc gr
-- [[3],[0,1],[2]]
--
-- @since 1.1.0.0
{-# INLINE scc #-}
scc :: Csr w -> V.Vector (VU.Vector Int)
scc = ACISCC.sccCsr

-- TODO: change scc to take arbitrary graph form

-- | \(O(n \log n + m)\) Returns the lexicographically smallest topological ordering of the given
-- graph.
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
{-# INLINABLE topSort #-}
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

-- | \(O(n + m)\) Returns a [block cut tree](https://en.wikipedia.org/wiki/Biconnected_component)
-- where super vertices represent each biconnected component.
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> -- 0---3---2
-- >>> -- +-1-+
-- >>> let n = 4
-- >>> let gr = Gr.build' n . Gr.swapDupe' $ VU.fromList [(0, 3), (0, 1), (1, 3), (3, 2)]
-- >>> let bct = blockCut n (gr `Gr.adj`)
-- >>> bct
-- Csr {nCsr = 6, mCsr = 5, startCsr = [0,0,0,0,0,2,5], adjCsr = [3,2,0,3,1], wCsr = [(),(),(),(),()]}
--
-- >>> V.generate (Gr.nCsr bct - n) ((bct `Gr.adj`) . (+ n))
-- [[3,2],[0,3,1]]
--
-- @since 1.1.1.0
{-# INLINABLE blockCut #-}
blockCut :: Int -> (Int -> VU.Vector Int) -> Csr ()
blockCut n gr = runST $ do
  low <- VUM.replicate n (0 :: Int)
  ord <- VUM.replicate n (0 :: Int)
  st <- B.new @_ @Int n
  used <- VUM.replicate n $ Bit False
  edges <- B.new @_ @(Int, Int {- TODO: correct capacity? -}) (2 * n)
  -- represents the bidirected component's index. also works as super vertex indices.
  next <- VUM.replicate 1 n

  let dfs k0 v p = do
        B.pushBack st v
        VGM.write used v $ Bit True
        VGM.write low v k0
        VGM.write ord v k0

        snd
          <$> VU.foldM'
            ( \(!child, !k) to -> do
                if to == p
                  then pure (child, k)
                  else do
                    Bit b <- VGM.read used to
                    if not b
                      then do
                        let !child' = child + 1
                        s <- B.length st
                        k' <- dfs k to v
                        lowTo <- VGM.read low to
                        VGM.modify low (min lowTo) v
                        ordV <- VGM.read ord v
                        when ((p == -1 && child' > 1) || (p /= -1 && lowTo >= ordV)) $ do
                          nxt <- VGM.unsafeRead next 0
                          VGM.unsafeWrite next 0 (nxt + 1)
                          B.pushBack edges (nxt, v)
                          len <- B.length st
                          for_ [1 .. len - s] $ \_ -> do
                            back <- fromJust <$> B.popBack st
                            B.pushBack edges (nxt, back)
                        pure (child', k')
                      else do
                        ordTo <- VGM.read ord to
                        VGM.modify low (min ordTo) v
                        pure (child, k)
            )
            (0 :: Int, k0 + 1)
            (gr v)

  _ <-
    VGM.ifoldM'
      ( \k v (Bit b) -> do
          if b
            then do
              pure k
            else do
              k' <- dfs k v (-1)
              st' <- B.unsafeFreeze st
              nxt <- VGM.unsafeRead next 0
              VGM.unsafeWrite next 0 (nxt + 1)
              VU.forM_ st' $ \x -> do
                B.pushBack edges (nxt, x)
              B.clear st
              pure k'
      )
      (0 :: Int)
      used

  n' <- VGM.unsafeRead next 0
  Csr.build' n' <$> B.unsafeFreeze edges

-- | \(O(n + m)\) Returns a [blocks (biconnected comopnents)](https://en.wikipedia.org/wiki/Biconnected_component)
-- of the graph.
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> -- 0---3---2
-- >>> -- +-1-+
-- >>> let n = 4
-- >>> let gr = Gr.build' n . Gr.swapDupe' $ VU.fromList [(0, 3), (0, 1), (1, 3), (3, 2)]
-- >>> Gr.blockCutComponents n (gr `Gr.adj`)
-- [[3,2],[0,3,1]]
--
-- @since 1.1.1.0
{-# INLINE blockCutComponents #-}
blockCutComponents :: Int -> (Int -> VU.Vector Int) -> V.Vector (VU.Vector Int)
blockCutComponents n gr =
  let bct = blockCut n gr
      d = nCsr bct - n
   in V.generate d ((bct `adj`) . (+ n))
