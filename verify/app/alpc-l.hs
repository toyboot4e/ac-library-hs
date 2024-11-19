{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

import AtCoder.LazySegTree qualified as LST
import Data.Bits
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Util

-- define newtype F
type FRepr = Bool

{- ORMOLU_DISABLE -}
newtype F = F FRepr deriving newtype (Eq, Ord, Show) ; unF :: F -> FRepr ; unF (F x) = x; newtype instance VU.MVector s F = MV_F (VU.MVector s FRepr) ; newtype instance VU.Vector F = V_F (VU.Vector FRepr) ; deriving instance VGM.MVector VUM.MVector F ; deriving instance VG.Vector VU.Vector F ; instance VU.Unbox F ;
{- ORMOLU_ENABLE -}

instance Semigroup F where
  {-# INLINE (<>) #-}
  (F !x1) <> (F !x2) = F (x1 `xor` x2)

instance Monoid F where
  {-# INLINE mempty #-}
  mempty = F False

instance LST.SegAct F X where
  {-# INLINE segAct #-}
  segAct (F False) acc = acc
  segAct (F True) (X (!n0, !n1, !nInv)) = X (n1, n0, n0 * n1 - nInv)

-- define newtype X
type XRepr = (Int, Int, Int)

{- ORMOLU_DISABLE -}
newtype X = X XRepr deriving newtype (Eq, Ord, Show) ; unX :: X -> XRepr ; unX (X x) = x ; newtype instance VU.MVector s X = MV_X (VU.MVector s XRepr) ; newtype instance VU.Vector X = V_X (VU.Vector XRepr) ; deriving instance VGM.MVector VUM.MVector X ; deriving instance VG.Vector VU.Vector X ; instance VU.Unbox X ;
{- ORMOLU_ENABLE -}

instance Semigroup X where
  {-# INLINE (<>) #-}
  (X (!n0, !n1, !nInv)) <> (X (!n0', !n1', !nInv')) = X (n0 + n0', n1 + n1', nInv + nInv' + n1 * n0')

instance Monoid X where
  {-# INLINE mempty #-}
  mempty = X (0, 0, 0)

-- verification-helper: PROBLEM https://atcoder.jp/contests/practice2/tasks/practice2_l
main :: IO ()
main = do
  (!_, !q) <- ints2
  xs <- ints
  qs <- VU.replicateM q $ do
    (!t, !l, !r) <- ints3
    pure (t, l - 1, r)

  seg <- LST.build $ VU.map (\case 0 -> X (1, 0, 0); 1 -> X (0, 1, 0)) xs
  res <- (`VU.mapMaybeM` qs) $ \case
    (1, !l, !r) -> do
      LST.applyIn seg l r $ F True
      pure Nothing
    (2, !l, !r) -> do
      X (!_, !_, !x) <- LST.prod seg l r
      pure $ Just x

  printBSB $ unlinesBSB res
