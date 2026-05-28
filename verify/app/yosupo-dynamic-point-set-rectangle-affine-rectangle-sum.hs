import AtCoder.Extra.LazyKdTree qualified as Kt
import AtCoder.Extra.Monoid.Affine1 (Affine1)
import AtCoder.Extra.Monoid.Affine1 qualified as Affine1
import AtCoder.Extra.Monoid.V2 (V2)
import AtCoder.Extra.Monoid.V2 qualified as V2
import AtCoder.ModInt qualified as M
import Data.Semigroup (Sum (..))
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Util

type Mint = M.ModInt998244353

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/dynamic_point_set_rectangle_affine_rectangle_sum
main :: IO ()
main = do
  (!n, !q) <- ints2
  xyws <- VU.replicateM n $ withLine ((,,) <$> intP <*> intP <*> (V2.new <$> mintP))
  qs <-
    VU.replicateM q $
      withLine $
        intP >>= \case
          0 -> (0 :: Int,,-1,-1) <$> ((,,,-1) <$> intP <*> intP <*> intP)
          1 -> (1 :: Int,,-1,-1) <$> ((,,-1,-1) <$> intP <*> intP)
          2 -> (2 :: Int,,-1,-1) <$> ((,,,) <$> intP <*> intP <*> intP <*> intP)
          3 -> (3 :: Int,,,) <$> ((,,,) <$> intP <*> intP <*> intP <*> intP) <*> intP <*> intP
          _ -> error "unreachable"

  -- initially blank, written later
  let xyws' =
        let (!ts, !tuples, !_, !_) = VU.unzip4 qs
            (!a, !b, !_, !_) = VU.unzip4 tuples
            zeroW = V2.zero
         in VU.mapMaybe
              (\case (0, !x, !y) -> Just (x, y, zeroW); _ -> Nothing)
              $ VU.zip3 ts a b
  let (!xs, !ys, !ws) = VU.unzip3 $ xyws VU.++ xyws'
  kt <- Kt.build @_ @(Affine1 Mint) @(V2 Mint) xs ys ws

  idx <- VUM.replicate 1 n
  res <- (`VU.mapMaybeM` qs) $ \case
    (0, (!_, !_, !w, !_), !_, !_) -> do
      i <- VGM.read idx 0
      VGM.write idx 0 $ i + 1
      Kt.write kt i $ V2.new $ M.new w
      pure Nothing
    (1, (!i, !w, !_, !_), !_, !_) -> do
      Kt.write kt i $ V2.new $ M.new w
      pure Nothing
    (2, (!l, !d, !r, !u), !_, !_) -> do
      Just . M.val . V2.unV2 <$> Kt.prod kt l r d u
    (3, (!l, !d, !r, !u), !a, !b) -> do
      Kt.applyIn kt l r d u $ Affine1.new (M.new a) (M.new b)
      pure Nothing
    _ -> error "unreachable"

  printBSB $ unlinesBSB res
