{-# LANGUAGE TypeFamilies #-}

import AtCoder.Extra.Math qualified as EM
import AtCoder.Extra.Monoid (Affine1 (..), SegAct (..))
import AtCoder.Extra.Monoid.Affine1 qualified as Affine1
import AtCoder.LazySegTree qualified as LST
import AtCoder.ModInt qualified as M
import Data.Monoid (Dual (..))
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Util

type Mint = M.ModInt998244353

modInt :: Int -> Mint
modInt = M.new

-- | Range set
type OpRepr = (Bool, Affine1 Mint)

instance Semigroup Op where
  {-# INLINE (<>) #-}
  new@(Op (!b1, !_)) <> old
    | not b1 = old
    | otherwise = new

instance Monoid Op where
  {-# INLINE mempty #-}
  mempty = Op (False, mempty)

instance SegAct Op Acc where
  {-# INLINE segAct #-}
  segAct = segActWithLength 1
  {-# INLINE segActWithLength #-}
  segActWithLength len (Op (!b, !f)) x
    | not b = x
    | len == 1 = Dual f
    | otherwise = Dual $ EM.power (<>) len f

type Acc = Dual (Affine1 Mint)

{- ORMOLU_DISABLE -}
newtype Op = Op OpRepr deriving newtype (Eq, Ord, Show) ; unOp :: Op -> OpRepr ; unOp (Op x) = x; newtype instance VU.MVector s Op = MV_Op (VU.MVector s OpRepr) ; newtype instance VU.Vector Op = V_Op (VU.Vector OpRepr) ; deriving instance VGM.MVector VUM.MVector Op ; deriving instance VG.Vector VU.Vector Op ; instance VU.Unbox Op ;
{- ORMOLU_ENABLE -}

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/range_set_range_composite
main :: IO ()
main = do
  (!n, !q) <- ints2
  xs <- VU.replicateM n $ do
    (!a, !b) <- ints2
    pure . Dual $ Affine1 (modInt a, modInt b)
  qs <- VU.replicateM q $ withLine $ do
    intP >>= \case
      0 -> (0 :: Int,,,,) <$> intP <*> intP <*> intP <*> intP
      1 -> (1,,,,-1) <$> intP <*> intP <*> intP
      _ -> error "unreachable"

  seg <- LST.build xs
  res <- (`VU.mapMaybeM` qs) $ \case
    (0, !l, !r, !a, !b) -> do
      LST.applyIn seg l r . Op $ (True, Affine1 (modInt a, modInt b))
      pure Nothing
    (1, !l, !r, !x, !_) -> do
      Dual f <- LST.prod seg l r
      pure . Just . M.val $ Affine1.act f (modInt x)
    _ -> error "unreachable"

  printBSB $ unlinesBSB res
