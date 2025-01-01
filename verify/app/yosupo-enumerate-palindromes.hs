import AtCoder.Extra.Bisect
import AtCoder.Extra.Monoid.RollingHash qualified as RH
import AtCoder.SegTree qualified as ST
import Data.ByteString.Char8 qualified as BS
import Data.Char (ord)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Dual (..))
import Data.Vector.Unboxed qualified as VU
import Util

type RH = RH.RollingHash 100 998244353

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/enumerate_palindromes
-- TODO: Refactor! It's too complicated
main :: IO ()
main = do
  s <- BS.getLine
  let !n = BS.length s
  seg <- ST.build @_ @RH . VU.map (RH.unsafeNew . ord) . VU.fromListN n $ BS.unpack s
  dualSeg <- ST.build @_ @(Dual RH) . VU.map (Dual . RH.unsafeNew . ord) . VU.fromListN n $ BS.unpack s

  let solve i0
        | even i0 = do
            -- | . . .
            let i = i0 `div` 2
            let maxLen = min i (n - 1 - i)
            d <- fromMaybe 0 <$> bisectLM 0 (maxLen + 1) (testAt i)
            pure $ 2 * d + 1
        | otherwise = do
            -- .|. . .
            let i = i0 `div` 2
            let maxLen = min (i + 1) (n - (i + 1))
            d <- fromMaybe 0 <$> bisectLM 0 (maxLen + 1) (testMid i)
            pure $ 2 * d
        where
          -- | . j.
          testAt _ 0 = pure True
          testAt i delta = do
            Dual hash1 <- ST.prod dualSeg (i - delta) (i + 1)
            hash2 <- ST.prod seg i (i + delta + 1)
            pure $ hash1 == hash2

          -- .|. .
          -- testMid _ 0 = pure False
          testMid i delta = do
            Dual hash1 <- ST.prod dualSeg ((i + 1) - delta) (i + 1)
            hash2 <- ST.prod seg (i + 1) (i + 1 + delta)
            pure $ hash1 == hash2

  res <- VU.mapM solve (VU.generate (2 * n - 1) id)
  printBSB $ unwordsBSB res
