import AtCoder.Extra.Tree.Lct qualified as Lct
import AtCoder.Internal.Assert qualified as ACIA
import Data.Semigroup (Max (..))
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/incremental_minimum_spanning_forest
main :: IO ()
main = do
  (!n, !q) <- ints2
  uvws <- VU.replicateM q ints3

  -- In order to handle edge weights, we treat edges as super vertices.
  lct <- Lct.new @_ @(Max (Int, Int)) (n + q)
  res <- VU.iforM uvws $ \iEdge (!u, !v, !w) -> do
    if u == v
      then do
        -- Don't add this loop edge
        pure iEdge
      else do
        -- Record edge weight:
        Lct.write lct (n + iEdge) (Max (w, iEdge))
        b <- Lct.same lct u v
        if b
          then do
            -- Remove edge with maximum weight. Note that w_i are distinct in this problem.
            m@(Max (!maxW, !iOldEdge)) <- Lct.prodPath lct u v
            let !_ = ACIA.runtimeAssert (m /= mempty) "not connected?"
            if maxW < w
              then do
                -- Don't add this edge.
                pure iEdge
              else do
                -- Unlink. Note that edges are treated as super vertices.
                Lct.cut lct u (n + iOldEdge)
                Lct.cut lct v (n + iOldEdge)
                -- Link.
                Lct.link lct u (n + iEdge)
                Lct.link lct v (n + iEdge)
                pure iOldEdge
          else do
            -- Just link. Note that edges are treated as super vertices.
            Lct.link lct u (n + iEdge)
            Lct.link lct v (n + iEdge)
            pure (-1)

  printBSB $ unwordsBSB res
