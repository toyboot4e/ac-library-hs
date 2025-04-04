module Tests.Extra.Graph where

import AtCoder.Extra.Graph qualified as Gr
import Control.Monad (unless)
import Control.Monad.Fix (fix)
import Data.List qualified as L
import Data.Vector qualified as V
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

reachableFlags :: Int -> (Int -> VU.Vector Int) -> Int -> VU.Vector Bool
reachableFlags n gr u0 = VU.create $ do
  vis <- VUM.replicate n False
  VUM.write vis u0 True
  flip fix u0 $ \loop u -> do
    VU.forM_ (gr u) $ \v -> do
      b <- VUM.exchange vis v True
      unless b $ do
        loop v
  pure vis

testTopSort :: Int -> Gr.Csr () -> VU.Vector Int -> Bool
testTopSort n gr vs =
  let reachables = V.generate n (reachableFlags n (gr `Gr.adj`))
   in and
        [ not $ reachables VG.! v VG.! u
          | iu <- [0 .. n - 1],
            let u = vs VG.! iu,
            iv <- [iu + 1 .. n - 1],
            let v = vs VG.! iv
        ]

-- | Tests lexicographically smallest topological ordering.
prop_topSort :: QC.Gen QC.Property
prop_topSort = do
  n <- QC.chooseInt (1, 3)
  dag <- genDag n
  let vs = Gr.topSort n (dag `Gr.adj`)
  let perms = map (VU.fromListN n) $ L.permutations [0 .. n - 1]
  pure $ vs QC.=== minimum (filter (testTopSort n dag) perms)

tests :: [TestTree]
tests =
  [ QC.testProperty "topSort" prop_topSort
  ]
