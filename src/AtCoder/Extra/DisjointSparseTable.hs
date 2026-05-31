{-# LANGUAGE RecordWildCards #-}

-- | Disjoint sparse table is a data structure that allows you to obtain monoid product over a
-- sequence of monoid values in an interval in \(O(1)\) time after \(O(n \log n)\) time setup.
--
-- In practice, Fenwick tree or segment tree can be faster, but if you need to process so many
-- monoid product queries, `DisjointSparseTable` can have better time complexity.
--
-- @since 1.6.0.0
module AtCoder.Extra.DisjointSparseTable where

import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Bit qualified as ACIB
import Data.Bits (bit, countLeadingZeros, xor)
import Data.Foldable (for_)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

-- Disjoint sparse table.
data DisjointSparseTable a = DisjointSparseTable
  { nDst :: {-# UNPACK #-} !Int,
    dataDst :: !(V.Vector (VU.Vector a))
  }

-- | \(O(n \log n)\) Creates a [`DisjointSparseTable`] over a sequense of monoid values.
build :: (Monoid a, VU.Unbox a) => VU.Vector a -> DisjointSparseTable a
-- TODO: handle length zero
build xs = DisjointSparseTable {..}
  where
    !nDst = VU.length xs
    !h = ACIB.ceilingLog2 nDst + 1
    !dataDst = V.unfoldrExactN h (VU.splitAt nDst) $ VU.create $ do
      vecData <- VUM.unsafeNew (h * nDst)
      let vec = V.unfoldrExactN h (VUM.splitAt nDst) vecData

      V.iforM_ vec $ \i row -> do
        -- Initialize
        VU.iforM_ xs $ \i x -> do
          VGM.write row i x

        -- interval: sum of suffix/prefix product lengths (<-|->)
        let interval = bit (i + 2)

        -- length: half of the interval (<-| or |->)
        let len = bit (i + 1)

        -- TODO: handle length < 2^x case
        for_ [0 .. nDst `div` interval - 1] $ \j -> do
          -- just copy
          let midL = interval * j + len - 1
          let midR = interval * j + len

          -- scan in-place [midR, midR + len)
          for_ [1 .. len - 1] $ \d -> do
            v1 <- VGM.read row (midR + d - 1)
            v2 <- VGM.read row (midR + d)
            VGM.write row (midR + d) $! v1 <> v2

          -- scan in-place [midL, midR)
          for_ [1 .. len - 1] $ \d -> do
            v1 <- VGM.read row (midL + d - 1)
            v2 <- VGM.read row (midL + d)
            VGM.write row (midL + d) $! v1 <> v2
      pure vecData

{-# INLINE msbOf #-}
msbOf :: Int -> Int
msbOf !x = 63 - countLeadingZeros x

-- | \(O(1)\) Calculates \(\Pi_{i \in [l, r)} {m_i}\) for the sequence of monoid values.
prod :: (Monoid a, VU.Unbox a) => DisjointSparseTable a -> Int -> Int -> a
prod DisjointSparseTable {..} l r
  | l == r = mempty
  | l + 1 == r = dataDst VG.! 0 VG.! l
  | otherwise =
      let k = msbOf (l `xor` (r - 1))
          !xl = dataDst VG.! k VG.! l
          !xr = dataDst VG.! k VG.! (r - 1)
       in xl <> xr
  where
    !_ = ACIA.checkInterval "AtCoder.Extra.DisjointSparseTable.prod" l r nDst
