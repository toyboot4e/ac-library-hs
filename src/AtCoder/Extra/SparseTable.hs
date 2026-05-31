{-# LANGUAGE RecordWildCards #-}

-- | Sparse table is a data structure that allows you to obtain monoid product over a sequence of
-- __ideomponent__ monoid values in an interval in \(O(1)\) time after \(O(n \log n)\) time setup.
-- Prefer @DisjointSparseTable@, because it allows a wider variety of monoids; this module is here
-- just for comparison, not for practical use.
--
-- In practice, Fenwick tree or segment tree can be faster, but if you need to process so many
-- monoid product queries, `SparseTable` can have better time complexity.
--
-- ==== __Example__
--
-- >>> import AtCoder.Extra.SparseTable qualified as Tbl
-- >>> import Data.Semigroup (Max (..))
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let tbl = Tbl.build @(Max Int) $ VU.fromList [0, 3, 1, 4, 2]
-- >>> Tbl.prod tbl 0 0
-- Max {getMax = -9223372036854775808}
--
-- >>> Tbl.prod tbl 0 3
-- Max {getMax = 3}
--
-- @since 1.6.0.0
module AtCoder.Extra.SparseTable
  ( SparseTable (..),
    build,
    prod,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Bit qualified as ACIB
import Data.Bits (bit)
import Data.Foldable (for_)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

-- | Sparse table.
--
-- @since 1.6.0.0
data SparseTable a = SparseTable
  { -- | The length of the sequence.
    --
    -- @since 1.6.0.0
    nSt :: {-# UNPACK #-} !Int,
    -- | data[i][j] stores monoid product of length 2^i at j.
    --
    -- @since 1.6.0.0
    dataSt :: !(V.Vector (VU.Vector a))
  }
  deriving (Show, Eq)

-- | \(O(n \log n)\) Creates a `SparseTable` over a sequence of __ideomponent__ monoid values.
--
-- @since 1.6.0.0
build :: (Monoid a, VU.Unbox a) => VU.Vector a -> SparseTable a
build xs
  | VU.null xs = SparseTable {nSt = 0, dataSt = V.empty}
build xs = SparseTable {..}
  where
    !nSt = VU.length xs
    !h = ACIB.ceilingLog2 nSt + 1
    !dataSt = V.unfoldrExactN h (VU.splitAt nSt) $ VU.create $ do
      vecData <- VUM.replicate (h * nSt) mempty
      let vec = V.unfoldrExactN h (VUM.splitAt nSt) vecData
      VU.copy (vec VG.! 0) xs
      VG.izipWithM_
        ( \i row1 row2 -> do
            let len = bit i
            for_ [0 .. nSt - 1 - len] $ \j -> do
              -- Each element in row1 corresponds to an interval of length 2^i at j:
              --   vec[i][j] = vec[i][j] <> vec[i][j + 2^i]
              x1 <- VGM.read row1 j
              x2 <- VGM.read row1 (j + len)
              VGM.write row2 j $! x1 <> x2
        )
        vec
        (V.tail vec)
      pure vecData

-- | \(O(1)\) Calculates \(\Pi_{i \in [l, r)} {m_i}\) for the sequence of __ideomponent__ monoid values.
prod :: (Monoid a, VU.Unbox a) => SparseTable a -> Int -> Int -> a
prod SparseTable {..} l r = case r - l of
  0 -> mempty
  1 -> dataSt VG.! 0 VG.! l
  _ ->
    -- The two intervals cover [l, r), maybe making a overlap
    let !lx = dataSt VG.! logLen VG.! l
        !rx = dataSt VG.! logLen VG.! (r - len)
     in lx <> rx
  where
    !_ = ACIA.checkInterval "AtCoder.Extra.SparseTable.prod" l r nSt
    logLen = ACIB.floorLog2 (r - l)
    len = bit logLen
