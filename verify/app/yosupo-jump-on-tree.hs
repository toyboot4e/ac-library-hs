import AtCoder.Extra.Graph qualified as Gr
import AtCoder.Extra.Tree.Hld qualified as Hld
import Data.Maybe (fromMaybe)
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/jump_on_tree
main :: IO ()
main = do
  (!n, !q) <- ints2
  es <- VU.replicateM (n - 1) ints2
  qs <- VU.replicateM q ints3
  let !gr = Gr.build' n $ Gr.swapDupe' es
  let !hld = Hld.new gr
  let !res =
        VU.map
          ( \(!u, !v, !i) ->
              fromMaybe (-1) $ Hld.jump hld u v i
          )
          qs
  printBSB $ unlinesBSB res
