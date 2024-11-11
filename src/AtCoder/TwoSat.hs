{-# LANGUAGE RecordWildCards #-}

-- | Solves 2-SAT.
--
-- For variables \(x_0, x_1, \cdots, x_{N - 1}\) and clauses with form
--
-- - \((x_i = f) \lor (x_j = g)\)
--
-- it decides whether there is a truth assignment that satisfies all clauses.
module AtCoder.TwoSat (TwoSat (..), new, addClause, satisfiable, answer) where

import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Scc qualified as ACISCC
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Bit (Bit (..))
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

-- | Creates a 2-SAT of \(n\) variables and \(0\) clauses.
--
-- = Constraints
-- - \(0 \leq n \leq 10^8\)
--
-- = Complexity
-- - \(O(n)\)
new :: (PrimMonad m) => Int -> m (TwoSat (PrimState m))
new nTs = do
  answerTs <- VUM.unsafeNew nTs
  sccTs <- ACISCC.new $ 2 * nTs
  return TwoSat {..}

-- | Adds a clause \((x_i = f) \lor (x_j = g)\).
--
-- = Constraints
-- - \(0 \leq i \lt n\)
-- - \(0 \leq j \lt n\)
--
-- = Complexity
-- - \(O(1)\) amortized.
addClause :: (HasCallStack, PrimMonad m) => TwoSat (PrimState m) -> Int -> Bool -> Int -> Bool -> m ()
addClause TwoSat {..} i f j g = do
  let !_ = ACIA.checkVertex "AtCoder.TwoSat.addClause" i nTs
  let !_ = ACIA.checkVertex "AtCoder.TwoSat.addClause" j nTs
  ACISCC.addEdge sccTs (2 * i + if f then 0 else 1) (2 * j + if g then 1 else 0)
  ACISCC.addEdge sccTs (2 * j + if g then 0 else 1) (2 * i + if f then 1 else 0)

-- | If there is a truth assignment that satisfies all clauses, it returns `True`. Otherwise, it
-- returns `False`.
--
-- = Constraints
-- - You may call it multiple times.
--
-- = Complexity
-- - \(O(n + m)\), where \(m\) is the number of added clauses.
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

-- | Returns a truth assignment that satisfies all clauses of the last call of `satisfiable`. If we
-- call it before calling `satisfiable` or when the last call of `satisfiable` returns `False`, it
-- returns the vector of length \(n\) with undefined elements.
--
-- = Complexity
-- - \(O(n)\)
answer :: (PrimMonad m) => TwoSat (PrimState m) -> m (VU.Vector Bit)
answer = VU.unsafeFreeze . answerTs
