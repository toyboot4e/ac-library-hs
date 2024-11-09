{-# LANGUAGE RecordWildCards #-}

module AtCoder.Scc (SccGraph (..), nScc, new, addEdge, scc) where

import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Scc qualified as ACISCC
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import GHC.Stack (HasCallStack)

newtype SccGraph s = SccGraph (ACISCC.SccGraph s)

nScc :: SccGraph s -> Int
nScc (SccGraph g) = ACISCC.nScc g

new :: (PrimMonad m) => Int -> m (SccGraph (PrimState m))
new n = SccGraph <$> ACISCC.new n

addEdge :: (HasCallStack, PrimMonad m) => SccGraph (PrimState m) -> (Int, Int) -> m ()
addEdge (SccGraph gr) e@(!from, !to) = do
  let n = ACISCC.nScc gr
  let !_ = ACIA.checkCustom "AtCoder.Scc.addEdge" "`from` vertex" from "the number of vertices" n
  let !_ = ACIA.checkCustom "AtCoder.Scc.addEdge" "`to` vertex" to "the number of vertices" n
  ACISCC.addEdge gr e

scc :: (PrimMonad m) => SccGraph (PrimState m) -> m (V.Vector (VU.Vector Int))
scc (SccGraph g) = ACISCC.scc g
