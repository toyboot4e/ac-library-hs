module Tests.Util
  ( myForAllShrink,
    laws,
    intervalGen,
    intervalGen',
    Dag (..),
    genDag,
    Tree (..),
    genTree,
  )
where

import AtCoder.Extra.Graph qualified as Gr
import AtCoder.Internal.Assert qualified as ACIA
import Data.Foldable (for_)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, typeRep)
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Test.QuickCheck.Classes qualified as QCC
import Test.QuickCheck.Property qualified as QC
import Test.Tasty
import Test.Tasty.QuickCheck qualified as QC

-- | Taken from `quickcheck-classes-base`.
myForAllShrink ::
  (QC.Arbitrary a, Show b, Eq b) =>
  Bool -> -- Should we show the RHS. It's better not to show it if the RHS is equal to the input.
  (a -> Bool) -> -- is the value a valid input
  (a -> [String]) -> -- show the 'a' values
  String -> -- show the LHS
  (a -> b) -> -- the function that makes the LHS
  String -> -- show the RHS
  (a -> b) -> -- the function that makes the RHS
  QC.Property
myForAllShrink displayRhs isValid showInputs name1 calc1 name2 calc2 =
  QC.MkProperty $
    QC.arbitrary >>= \x ->
      QC.unProperty $
        QC.shrinking QC.shrink x $ \x' ->
          let b1 = calc1 x'
              b2 = calc2 x'
              sb1 = show b1
              sb2 = show b2
              description = "  Description: " ++ name1 ++ " = " ++ name2
              err = description ++ "\n" ++ unlines (map ("  " ++) (showInputs x')) ++ "  " ++ name1 ++ " = " ++ sb1 ++ (if displayRhs then "\n  " ++ name2 ++ " = " ++ sb2 else "")
           in isValid x' QC.==> QC.counterexample err (b1 == b2)

-- | Taken from `quickcheck-classes-base`.
laws :: forall a. (Typeable a) => [Proxy a -> QCC.Laws] -> TestTree
laws =
  testGroup (show (typeRep (Proxy @a)))
    . map
      ( \f ->
          let QCC.Laws name pairs = f (Proxy @a)
           in testGroup name (map (uncurry QC.testProperty) pairs)
      )

-- | Returns an interval [l, r) in [0, n)
intervalGen :: Int -> QC.Gen (Int, Int)
intervalGen = intervalGen' 0

-- | Returns an interval [l, r) in [0, n)
intervalGen' :: Int -> Int -> QC.Gen (Int, Int)
intervalGen' xl xr = do
  l <- QC.chooseInt (xl, xr)
  r <- QC.chooseInt (l, xr)
  pure (l, r)

genDag :: forall w. (QC.Arbitrary w, VU.Unbox w) => Int -> QC.Gen (Gr.Csr w)
genDag n = do
  edges <- VU.fromList <$> QC.sublistOf [(u, v) | u <- [0 .. n - 1], v <- [u + 1 .. n - 1]]
  verts <- VU.fromList <$> QC.shuffle [0 .. n - 1]
  ws <- VU.fromList <$> QC.vectorOf n (QC.arbitrary @w)
  let es = VU.zipWith (\(!u, !v) !w -> (verts VG.! u, verts VG.! v, w)) edges ws
  pure $ Gr.build n es

-- | Newtype for generating a random DAG.
newtype Dag w = Dag (Gr.Csr w)

instance (QC.Arbitrary w, VU.Unbox w) => QC.Arbitrary (Dag w) where
  arbitrary = do
    QC.Positive n <- QC.arbitrary
    Dag <$> genDag n

-- TODO: commit the upper code first

-- | Prüfer sequence: https://en.wikipedia.org/wiki/Pr%C3%BCfer_sequence
genTreeEdge :: Int -> QC.Gen (VU.Vector (Int, Int))
genTreeEdge n = case n of
  1 -> pure VU.empty
  2 -> pure $ VU.singleton (0, 1)
  _ -> do
    prufer <- QC.vectorOf (n - 2) (QC.chooseInt (0, n - 1))
    pure $ VU.create $ do
      -- sum(deg) = n + (n - 2) at the beginning
      deg <- VUM.replicate n (1 :: Int)
      for_ prufer $ \v -> do
        VGM.modify deg (+ 1) v

      edges <- VUM.unsafeNew (n - 1)

      -- sum(deg) will be 2(n - 1) - 2(n - 2) = 2
      for_ (zip [0 :: Int ..] prufer) $ \(!i, !u) -> do
        -- NOTE: At the beginning, deg[u] > 1, and deg[i] is never decreaesed until iterated
        !v <- fromJust . VU.findIndex (== 1) <$> VU.unsafeFreeze deg
        -- So we're sure u /= v
        let !_ = ACIA.runtimeAssert (u /= v) "u /= v"
        VGM.write edges i (u, v)
        VGM.modify deg (subtract 1) u
        VGM.modify deg (subtract 1) v

      -- The last two vertices with degree one make up the last edge
      uv <- VU.findIndices (== 1) <$> VU.unsafeFreeze deg
      let !_ = ACIA.runtimeAssert (VU.length uv == 2) "not found uv?"
      VGM.write edges (n - 1) (uv VG.! 0, uv VG.! 1)

      -- done
      pure edges

genTree :: forall w. (QC.Arbitrary w, VU.Unbox w) => Int -> QC.Gen (Gr.Csr w)
genTree n = do
  es <- genTreeEdge n
  ws <- VU.fromList <$> QC.vectorOf (n - 1) (QC.arbitrary @w)
  pure . Gr.build n . Gr.swapDupe $ VU.zipWith (\(!u, !v) !w -> (u, v, w)) es ws

-- | Newtype for generating a random tree.
newtype Tree w = Tree (Gr.Csr w)

instance (QC.Arbitrary w, VU.Unbox w) => QC.Arbitrary (Tree w) where
  arbitrary = do
    QC.Positive n <- QC.arbitrary
    Tree <$> genTree n
