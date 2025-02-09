import AtCoder.Extra.Seq.Map qualified as M
import Control.Monad (when)
import Data.Bool (bool)
import Data.ByteString.Char8 qualified as BS
import Data.Foldable (for_)
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/predecessor_problem
main :: IO ()
main = do
  (!n, !q) <- ints2
  t <- BS.getLine
  qs <- VU.replicateM q ints2

  -- m <- M.new @_ @() @Int @() (n + q)
  -- for_ (zip [0 :: Int ..] (BS.unpack t)) $ \(!i, !c) -> do
  --   when (c == '1') $ do
  --     M.insert m i ()

  m <- M.build @_ @() @Int @() (n + q) . VU.map (,()) . VU.elemIndices '1' . VU.fromList $ BS.unpack t

  res <- (`VU.mapMaybeM` qs) $ \case
    (0, !k) -> do
      M.insert m k ()
      pure Nothing
    (1, !k) -> do
      M.delete_ m k
      pure Nothing
    (2, !k) -> do
      Just . bool (0 :: Int) 1 <$> M.member m k
    (3, !k) -> do
      Just . maybe (-1) fst <$> M.lookupGE m k
    (4, !k) -> do
      Just . maybe (-1) fst <$> M.lookupLE m k
    _ -> error "unreachable"

  printBSB $ unlinesBSB res
