import AtCoder.Extra.Graph qualified as Gr
import AtCoder.Extra.Tree qualified as Tree
import AtCoder.Extra.Monoid.Mat2x2 qualified as Mat2x2
import AtCoder.Extra.Monoid.V2 qualified as V2
import AtCoder.ModInt qualified as M
import Data.Vector.Generic qualified as VG
import Data.Vector.Unboxed qualified as VU
import Util

type Mint = M.ModInt 998244353

modInt :: Int -> Mint
modInt = M.new

-- | Edge weight type.
type W = Mat2x2.Mat2x2 Mint

-- | Vertex value type.
type X = V2.V2 Mint

type F = V2.V2 Mint

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/tree_path_composite_sum
main :: IO ()
main = do
  n <- int
  xs <- VU.map modInt <$> ints
  es <- VU.replicateM (n - 1) $ do
    (\(!u, !v, !a, !b) -> (u, v, Mat2x2.new (modInt a) (modInt b))) <$> ints4

  let gr = Gr.build n $ Gr.swapDupe es
  let res = Tree.foldReroot n (gr `Gr.adjW`) valAt toF act
        where
          valAt :: Int -> X
          valAt v = V2.new $ xs VG.! v
          toF :: X -> (Int, W) -> F
          toF !op (!_, !affine) = Mat2x2.act affine op
          act :: F -> X -> X
          act = (<>)

  printBSB . unwordsBSB $ VU.map (M.val . V2.unV2) res
