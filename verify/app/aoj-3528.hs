import AtCoder.Extra.Graph qualified as Gr
import AtCoder.Extra.Ix0
import Control.Monad (replicateM)
import Data.ByteString.Char8 qualified as BS
import Data.Maybe (fromJust)
import Data.Vector.Unboxed qualified as VU
import Util

ortho4 :: VU.Vector (Int, Int)
ortho4 = VU.fromList [(0, 1), (0, -1), (1, 0), (-1, 0)]

adjOrtho4 :: (Int, Int) -> VU.Vector (Int, Int)
adjOrtho4 (!y, !x) = VU.map (add2 (y, x)) ortho4

add2 :: (Int, Int) -> (Int, Int) -> (Int, Int)
add2 (!y, !x) (!dy, !dx) = (y + dy, x + dx)

-- verification-helper: PROBLEM https://onlinejudge.u-aizu.ac.jp/problems/3528
main :: IO ()
main = do
  bnd0@(!h, !w) <- ints2
  gr <- BS.concat <$> replicateM h BS.getLine

  let s = (`divMod` w) . fromJust $ BS.elemIndex 'S' gr
  let ti = fromJust $ BS.elemIndex 'G' gr
  let res = Gr.bfs01 bnd0 (4 * h * w) grF $ VU.singleton (s, 0)
        where
          grF (!y, !x) = case c of
            'G' -> VU.empty
            'L' -> filter1 $ VU.singleton ((y, x - 1), 0)
            'R' -> filter1 $ VU.singleton ((y, x + 1), 0)
            'U' -> filter1 $ VU.singleton ((y - 1, x), 0)
            'D' -> filter1 $ VU.singleton ((y + 1, x), 0)
            _ ->
              VU.map (,1) . VU.filter p $ adjOrtho4 (y, x)
            where
              p = (&&) <$> inRange0 bnd0 <*> (/= '#') . BS.index gr . index0 bnd0
              c = BS.index gr $ index0 bnd0 (y, x)
              filter1 = VU.filter (p . fst)

  print $ res VU.! ti
