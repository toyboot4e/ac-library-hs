import AtCoder.Dsu qualified as Dsu
import AtCoder.Extra.Graph qualified as Gr
import AtCoder.Extra.Tree qualified as Tree
import Control.Monad.ST
import Control.Monad.ST (runST)
import Data.Functor.Identity (runIdentity)
import Data.Maybe (isJust)
import Data.Ord (comparing)
import Data.Set qualified as S
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/minimum_spanning_tree
main :: IO ()
main = do
  (!n, !m) <- ints2
  uvws <- VU.replicateM m ints3
  let (!w, !edgeUse) = Tree.mst n uvws
  print w
  printBSB . unwordsBSB $ VU.findIndices edgeUse
