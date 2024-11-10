{-# LANGUAGE RecordWildCards #-}

module AtCoder.Scc (SccGraph (..), nScc, new, addEdge, scc) where

import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Scc qualified as ACISCC
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import GHC.Stack (HasCallStack)

newtype SccGraph s = SccGraph (ACISCC.SccGraph s)

--
-- = Constraints
--
-- = Complexity
nScc :: SccGraph s -> Int
nScc (SccGraph g) = ACISCC.nScc g

-- | Creates a directed graph with \(n\) vertices and \(0\) edges.
--
-- = Constraints
-- - \(0 \leq n\)
--
-- = Complexity
-- - \(O(n)\)
new :: (PrimMonad m) => Int -> m (SccGraph (PrimState m))
new n = SccGraph <$> ACISCC.new n

-- | Adds a directed edge from the vertex @from@ to the vertex @to@.
--
-- = Constraints
-- - \(0 \leq \mathrm{from} \lt n\)
-- - \(0 \leq \mathrm{to} \lt n\)
--
-- = Complexity
-- - \(O(1)\) amortized
addEdge :: (HasCallStack, PrimMonad m) => SccGraph (PrimState m) -> (Int, Int) -> m ()
addEdge (SccGraph gr) e@(!from, !to) = do
  let n = ACISCC.nScc gr
  let !_ = ACIA.checkCustom "AtCoder.Scc.addEdge" "`from` vertex" from "the number of vertices" n
  let !_ = ACIA.checkCustom "AtCoder.Scc.addEdge" "`to` vertex" to "the number of vertices" n
  ACISCC.addEdge gr e

-- | Returns the list of the "list of the vertices" that satisfies the following.
--
-- Each vertex is in exactly one "list of the vertices".
-- Each "list of the vertices" corresponds to the vertex set of a strongly connected component. The order of the vertices in the list is undefined.
-- The list of "list of the vertices" are sorted in topological order, i.e., for two vertices \(u, v\) in different strongly connected components, if there is a directed path from \(u\) to \(v\), the list containing \(u\) appears earlier than the list containing \(v\).
--
-- = Complexity
-- - \(O(n + m)\), where \(m\) is the number of added edges.
scc :: (PrimMonad m) => SccGraph (PrimState m) -> m (V.Vector (VU.Vector Int))
scc (SccGraph g) = ACISCC.scc g
