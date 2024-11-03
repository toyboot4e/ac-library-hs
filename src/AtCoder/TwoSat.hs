{-# LANGUAGE RecordWildCards #-}

module AtCoder.TwoSat (TwoSat(..), new, addClause, satisfiable, answer) where

import AtCoder.Internal.Assert (runtimeAssert)
import AtCoder.Internal.Scc qualified as ACISCC
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bit (Bit(..))
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)

data TwoSat s = TwoSat
  { nTs :: {-# UNPACK #-} !Int,
    answerTs :: !(VUM.MVector s Bit),
    sccTs :: !(ACISCC.SccGraph s)
  }

new :: (PrimMonad m) => Int -> m (TwoSat (PrimState m))
new nTs = do
  answerTs <- VUM.unsafeNew nTs
  sccTs <- ACISCC.new $ 2 * nTs
  return TwoSat {..}

addClause :: (HasCallStack,PrimMonad m) => TwoSat (PrimState m) -> Int -> Bool -> Int -> Bool -> m ()
addClause TwoSat {..} i f j g = do
  let !_ = runtimeAssert (0 <= i && i < nTs) $ "addClause: `i` vertex out of bounds (`" ++ show i ++ "` over the number of vertices `" ++ show nTs ++ "`)"
  let !_ = runtimeAssert (0 <= j && j < nTs) $ "addClause: `j` vertex out of bounds (`" ++ show j ++ "` over the number of vertices `" ++ show nTs ++ "`)"
  ACISCC.addEdge sccTs (2 * i + if f then 0 else 1, 2 * j + if g then 1 else 0)
  ACISCC.addEdge sccTs (2 * j + if g then 0 else 1, 2 * i + if f then 1 else 0)

satisfiable :: (PrimMonad m) => TwoSat (PrimState m) -> m Bool
satisfiable TwoSat {..} = do
  (!_, !ids) <- ACISCC.sccIds sccTs
  let inner i
        | i >= nTs = return True
        | ids VG.! (2 * i) == ids VG.! (2 * i + 1) = return False
        | otherwise = do
            VGM.write answerTs i . Bit $ ids VG.! (2 * i) < ids VG.! (2 * i + 1)
            inner (i + 1)
  inner 0

answer :: (PrimMonad m) => TwoSat (PrimState m) -> m (VU.Vector Bit)
answer = VU.unsafeFreeze . answerTs
