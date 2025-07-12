-- This is a possible template of LazySegTree monoid instances for AtCoder contests.
-- This code is copy-pasted to the lazy segtree document's example.
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}

import AtCoder.LazySegTree (SegAct (..))
import AtCoder.LazySegTree qualified as LST
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

{- ORMOLU_DISABLE -}
-- | `F` is a custom monoid action, defined as a newtype of `FRepr`.
newtype F = F FRepr deriving newtype (Eq, Ord, Show) ; unF :: F -> FRepr ; unF (F x) = x ; newtype instance VU.MVector s F = MV_F (VU.MVector s FRepr) ; newtype instance VU.Vector F = V_F (VU.Vector FRepr) ; deriving instance VGM.MVector VUM.MVector F ; deriving instance VG.Vector VU.Vector F ; instance VU.Unbox F ;
{- ORMOLU_ENABLE -}

-- | Affine: f x = a * x + b
type FRepr = (Int, Int)

instance Semigroup F where
  -- @new <> old@
  {-# INLINE (<>) #-}
  (F (!a1, !b1)) <> (F (!a2, !b2)) = F (a1 * a2, a1 * b2 + b1)

instance Monoid F where
  {-# INLINE mempty #-}
  mempty = F (1, 0)

{- ORMOLU_DISABLE -}
-- | `X` is a custom acted monoid, defined as a newtype of `XRepr`.
newtype X = X XRepr deriving newtype (Eq, Ord, Show) ; unX :: X -> XRepr ; unX (X x) = x; newtype instance VU.MVector s X = MV_X (VU.MVector s XRepr) ; newtype instance VU.Vector X = V_X (VU.Vector XRepr) ; deriving instance VGM.MVector VUM.MVector X ; deriving instance VG.Vector VU.Vector X ; instance VU.Unbox X ;
{- ORMOLU_ENABLE -}

-- | Acted `Int` (same as `Sum Int`).
type XRepr = Int

deriving instance Num X -- in our case `X` is a `Num`.

instance Semigroup X where
  {-# INLINE (<>) #-}
  (X x1) <> (X x2) = X $! x1 + x2

instance Monoid X where
  {-# INLINE mempty #-}
  mempty = X 0

-- Define the action:
instance SegAct F X where
  -- {-# INLINE segAct #-}
  -- segAct len (F (!a, !b)) (X x) = X $! a * x + b
  {-# INLINE segActWithLength #-}
  segActWithLength len (F (!a, !b)) (X x) = X $! a * x + len * b

----------------------------------------------------------------------------------------------------
-- The code above is a possible template.
-- The code below are tests (without additional imports to the template).
----------------------------------------------------------------------------------------------------

expect :: (Eq a, Show a) => String -> a -> a -> ()
expect msg a b
  | a == b = ()
  | otherwise = error $ msg ++ ": expected " ++ show a ++ ", found " ++ show b

main :: IO ()
main = do
  seg <- LST.build @_ @F @X $ VU.map X $ VU.fromList [1, 2, 3, 4]
  LST.applyIn seg 1 3 $ F (2, 1) -- [1, 5, 7, 4]
  LST.write seg 3 $ X 10 -- [1, 5, 7, 10]
  LST.modify seg (+ (X 1)) 0 -- [2, 5, 7, 10]
  !_ <- (expect "test 1" (X 5)) <$> LST.read seg 1
  !_ <- (expect "test 2" (X 14)) <$> LST.prod seg 0 3 -- reads an interval [0, 3)
  !_ <- (expect "test 3" (X 24)) <$> LST.allProd seg
  !_ <- (expect "test 4" 2) <$> LST.maxRight seg 0 (<= (X 10)) -- sum [0, 2) = 7 <= 10
  !_ <- (expect "test 5" 3) <$> LST.minLeft seg 4 (<= (X 10)) -- sum [3, 4) = 10 <= 10
  putStrLn "=> test passed!"
