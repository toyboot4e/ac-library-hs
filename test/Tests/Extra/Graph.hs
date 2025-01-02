module Tests.Extra.Graph where

import AtCoder.Extra.Graph qualified as Gr
import AtCoder.Internal.Buffer qualified as B
import Control.Monad (unless)
import Control.Monad.Fix (fix)
import Control.Monad.ST (runST)
import Data.List qualified as L
import Data.Vector.Generic qualified as VG
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Test.Tasty
import Test.Tasty.QuickCheck as QC

genDag :: Int -> QC.Gen (Gr.Csr ())
genDag n = do
  edges <- VU.fromList <$> QC.sublistOf [(u, v) | u <- [0 .. n - 1], v <- [u + 1 .. n - 1]]
  verts <- VU.fromList <$> QC.shuffle [0 .. n - 1]
  pure $ Gr.build n $ VU.map (\(!u, !v) -> (verts VG.! u, verts VG.! v, ())) edges

dfs :: Int -> (Int -> VU.Vector Int) -> Int -> VU.Vector Int
dfs n gr u0 = runST $ do
  buf <- B.new n
  vis <- VUM.replicate n False
  flip fix u0 $ \loop u -> do
    VU.forM_ (gr u) $ \v -> do
      b <- VUM.read vis v
      unless b $ do
        B.pushBack buf v
        loop v
  B.unsafeFreeze buf

testTopSort :: Int -> Gr.Csr () -> VU.Vector Int -> Bool
testTopSort n gr vs = and
    [ VU.notElem v (dfs n (gr `Gr.adj`) u)
      | u <- (vs VG.!) <$> [0 .. n - 1],
        v <- (vs VG.!) <$> [u + 1 .. n - 1]
    ]

-- | Tests lexicographically smallest topological ordering.
prop_topSort :: QC.Gen QC.Property
prop_topSort = do
  n <- QC.chooseInt (1, 8)
  dag <- genDag n
  let vs = Gr.topSort n (dag `Gr.adj`)
  let perms = map (VU.fromListN n) $ L.permutations [0 .. n - 1]
  pure $ vs QC.=== head (filter (testTopSort n dag) perms)

tests :: [TestTree]
tests =
  [ QC.testProperty "topSort" prop_topSort
  ]
