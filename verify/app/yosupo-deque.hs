import AtCoder.Internal.Queue qualified as Q
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/deque
main :: IO ()
main = do
  q <- int
  qs <-
    VU.replicateM q $
      withLine $
        intP >>= \case
          2 -> pure (2, -1)
          3 -> pure (3, -1)
          a -> (a,) <$> intP

  deq <- Q.newDeque q
  res <- (`VU.mapMaybeM` qs) $ \case
    (0, !x) -> do
      Q.pushFront deq x
      pure Nothing
    (1, !x) -> do
      Q.pushBack deq x
      pure Nothing
    (2, !_) -> do
      Q.popFront_ deq
      pure Nothing
    (3, !_) -> do
      Q.popBack_ deq
      pure Nothing
    (4, !i) -> do
      Just <$> Q.readFront deq i
    _ -> error "unreachable"

  printBSB $ unlinesBSB res
