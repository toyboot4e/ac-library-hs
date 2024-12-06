{-# LANGUAGE LambdaCase #-}

import AtCoder.LazySegTree qualified as LST
import AtCoder.LazySegTree.Monoid (Affine2d (..))
import AtCoder.ModInt qualified as M
import Data.Semigroup (Sum (..))
import Data.Vector.Unboxed qualified as VU
import Util

type Mint = M.ModInt998244353

modInt :: Int -> Mint
modInt = M.new

-- | \(O(W)\) Calculates @s^n@ by @n@ (N > 0) times using the binary lifting technique.
{-# INLINE power #-}
-- TODO: move to extra module.
power :: Int -> (a -> a -> a) -> a -> a
power n0 op x1
  | n0 <= 0 = errorWithoutStackTrace "power: positive multiplier expected"
  | otherwise = f x1 n0
  where
    f !x !n
      | even n = f (x `op` x) (n .>>. 1)
      | n == 1 = x
      | otherwise = g (x `op` x) (n .>>. 1) x
    g !x !n !z
      | even n = g (x `op` x) (n .>>. 1) z
      | n == 1 = x `op` z
      | otherwise = g (x `op` x) (n .>>. 1) (x `op` z)

-- | Add
type OpRepr = Affine2d MyModInt

instance Semigroup Op where
  {-# INLINE (<>) #-}
  new <> _old = new

instance Monoid Op where
  -- REMARK: be sure to implement identity operator
  {-# INLINE mempty #-}
  mempty = Op (Affine2d (ModInt (-1), ModInt (-1)))

instance SemigroupAction Op Acc where
  {-# INLINE sact #-}
  sact = segAct

instance SegmentAction Op Acc where
  {-# INLINE segActWithLength #-}
  segActWithLength len op@(Op f) x
    | op == mempty = x
    | len == 1 = Dual f
    | otherwise = Dual $ power (<>) len f

type Acc = Dual (Affine2d MyModInt)

{- ORMOLU_DISABLE -}
newtype Op = Op OpRepr deriving newtype (Eq, Ord, Show) ; unOp :: Op -> OpRepr ; unOp (Op x) = x; newtype instance VU.MVector s Op = MV_Op (VU.MVector s OpRepr) ; newtype instance VU.Vector Op = V_Op (VU.Vector OpRepr) ; deriving instance GM.MVector VUM.MVector Op ; deriving instance G.Vector VU.Vector Op ; instance VU.Unbox Op ;
{- ORMOLU_ENABLE -}

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/range_set_range_composite
main :: IO ()
main = do
  (!_, !q) <- ints2
  xs <- VU.map (\(!a, !b) -> Dual (Affine2d (modInt a, modInt b))) <$> ints
  qs <- VU.replicateM q $ withLine $ do
    intP >>= \case
      0 -> (0 :: Int,,,,) <$> intP <*> intP <*> intP <*> intP
      1 -> (1,,,,-1) <$> intP <*> intP <*> intP
      _ -> error "unreachable"

  seg <- LST.build xs
  res <- (`VU.mapMaybeM` qs) $ \case
    (0, !l, !r, !a, !b) -> do
      LST.applyIn seg l r . Op $ Affine2d (modInt a, modInt b)
      return Nothing
    (1, !l, !r, !x, !_) -> do
      Dual f <- LST.prod seg l r
      pure . Just . M.val x $ segAct f x
    _ -> error "unreachable"

  printBSB $ unlinesBSB res
