{-# LANGUAGE RecordWildCards #-}

-- | Sparse table is a data structure that allows you to obtain monoid product of a ideomponent
-- monoids in an interval in \(O(1)\) time after \(O(n \log n)\) time setup.
--
-- In practice, Fenwick tree or segment tree can be faster, but if you need to process so many
-- monoid product queries, `SparseTable` can have better time complexity.
--
-- ==== __Example__
--
-- >>> import AtCoder.Extra.SparseTable qualified as Tbl
-- >>> import Data.Semigroup (Max (..))
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let tbl = Tbl.new @(Max Int) $ VU.fromList [0, 3, 1, 4, 2]
-- >>> Tbl.prod tbl 0 3
-- Max {getMax = 3}
--
-- @1.6.0.0
module AtCoder.Extra.SparseTable
  ( SparseTable,
    new,
    prod,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Bit qualified as ACIB
import Control.Monad.ST (runST)
import Data.Bits (bit)
import Data.Foldable (for_)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

data SparseTable a = SparseTable
  { -- | The length of the sequence.
    nSt :: {-# UNPACK #-} !Int,
    -- | data[i][j] stores monoid product of length 2^i at j.
    dataSt :: !(V.Vector (VU.Vector a))
  }

-- | \(O(n \log n)\) Creates `SparseTable` for a sequence of ideomponent monoid values.
new :: (Monoid a, VU.Unbox a) => VU.Vector a -> SparseTable a
new xs = runST $ do
  let nSt = VU.length xs
  let h = ACIB.ceilingLog2 nSt

  vec <- V.unfoldrExactN h (VUM.splitAt nSt) <$> VUM.replicate (h * nSt) mempty
  -- TODO: use VUM.copy
  VU.iforM_ xs $ \i x -> do
    VGM.write (vec VG.! 0) i x
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

  dataSt <- V.mapM VU.unsafeFreeze vec
  pure SparseTable {..}

-- | \(O(1)\) Calculates \(\Pi{m_l, .., m_{r_1}}\) for ideomponent monoid.
prod :: (Monoid a, VU.Unbox a) => SparseTable a -> Int -> Int -> a
prod SparseTable {..} l r
  | r - l == 0 = mempty
  | otherwise =
      -- The two intervals cover [l, r), maybe making a overlap
      let !lx = dataSt VG.! logLen VG.! l
          !rx = dataSt VG.! logLen VG.! (r - len)
       in lx <> rx
  where
    !_ = ACIA.checkInterval "AtCoder.Extra.SparseTable" l r nSt
    logLen = ACIB.floorLog2 (r - l)
    len = bit logLen
