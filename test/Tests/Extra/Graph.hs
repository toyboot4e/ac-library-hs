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
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Tests.Util (genDag)

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
  dag <- genDag @() n
  let vs = Gr.topSort n (dag `Gr.adj`)
  let perms = map (VU.fromListN n) $ L.permutations [0 .. n - 1]
  pure $ vs QC.=== minimum (filter (testTopSort n dag) perms)

genComplexEdges :: Int -> QC.Gen (VU.Vector (Int, Int, Int))
genComplexEdges n = do
  m <- QC.chooseInt (1, 2 * n * n)
  (VU.fromList <$>) . QC.vectorOf m $ do
    u <- QC.chooseInt (0, n - 1)
    v <- QC.chooseInt (0, n - 1)
    w <- QC.arbitrary @Int
    pure (u, v, w)

prop_floydWarshall :: QC.Gen QC.Property
prop_floydWarshall = do
  -- n <- QC.chooseInt (1, 16)
  let n = 4
  es <- genComplexEdges n
  let !undefW = maxBound `div` 2 :: Int
  let (!distFw, !_prevFw) = Gr.trackingFloydWarshall n es undefW
  let gr = Gr.build n es
  let !bell = V.generate n $ Gr.trackingBellmanFord n (Gr.adjW gr) undefW . VU.singleton . (,0)
  pure $
    QC.counterexample (show (n, es)) $
      QC.conjoin
        [ case bell VG.! u of
            -- TODO: assertion function?
            Nothing -> any (\vtx -> distFw VG.! (n * vtx + vtx) < 0) [0 .. n - 1] QC.=== True
            Just (!distB, !_prevB) ->
              QC.conjoin
                [ distFw VG.! (n * u + v) QC.=== distB VG.! v
                -- TODO: Shortest paths cannot be uniqueified, so other test would be suitable
                -- , Gr.constructPathFromRootNN prevFw u v QC.=== Gr.constructPathFromRoot prevB v
                ]
          | u <- [0 .. n - 1],
            v <- [0 .. n - 1]
        ]

unit_loopPathConstruction :: TestTree
unit_loopPathConstruction = testCase "loop path reconstruction" $ do
  let parents = VU.fromList [3, 0, 1, 2]
  let path = Gr.constructPathFromRoot parents 3
  path @?= VU.fromList [0, 1, 2, 3]

unit_one :: TestTree
unit_one = testCase "one" $ do
  let !gr = Gr.build @Int 1 VU.empty
  Gr.topSort 1 (Gr.adj gr) @?= VU.singleton 0
  Gr.scc gr @?= V.singleton (VU.singleton 0)
  Gr.rev gr @?= gr
  Gr.findCycleDirected gr @?= Nothing
  Gr.findCycleUndirected gr @?= Nothing
  Gr.connectedComponents 1 (Gr.adj gr) @?= V.singleton (VU.singleton 0)
  (VU.length <$> (Gr.bipartiteVertexColors 1 (Gr.adj gr))) @?= Just 1
  -- Gr.blockCut 1 (Gr.adj gr)
  -- Gr.blockCutComponents 1 (Gr.adj gr)
  Gr.bfs 1 (Gr.adjW gr) (-1 :: Int) (VU.singleton (0, 7)) @?= (VU.singleton 7)
  Gr.trackingBfs 1 (Gr.adjW gr) (-1 :: Int) (VU.singleton (0, 7)) @?= (VU.singleton 7, VU.singleton (-1))
  Gr.bfs01 1 (Gr.adjW gr) {- capacity -} 0 (VU.singleton (0, 7)) @?= (VU.singleton 7)
  Gr.trackingBfs01 1 (Gr.adjW gr) {- capacity -} 0 (VU.singleton (0, 7)) @?= (VU.singleton 7, VU.singleton (-1))
  Gr.dijkstra 1 (Gr.adjW gr) 0 (-1 :: Int) (VU.singleton (0, 7)) @?= (VU.singleton 7)
  Gr.trackingDijkstra 1 (Gr.adjW gr) 0 (-1 :: Int) (VU.singleton (0, 7)) @?= (VU.singleton 7, VU.singleton (-1))
  Gr.bellmanFord 1 (Gr.adjW gr) (-1 :: Int) (VU.singleton (0, 7)) @?= Just (VU.singleton 7)
  Gr.trackingBellmanFord 1 (Gr.adjW gr) (-1 :: Int) (VU.singleton (0, 7)) @?= Just (VU.singleton 7, VU.singleton (-1))
  Gr.floydWarshall 1 VU.empty (maxBound `div` 2 :: Int) @?= VU.singleton 0
  Gr.trackingFloydWarshall 1 VU.empty (maxBound `div` 2 :: Int) @?= (VU.singleton 0, VU.singleton (-1))
  (!fw, !prev) <- Gr.newTrackingFloydWarshall 1 VU.empty (maxBound `div` 2 :: Int)
  let test = do
        fw' <- VU.unsafeFreeze fw
        prev' <- VU.unsafeFreeze prev
        (fw', prev') @?= (VU.singleton 0, VU.singleton (-1))
  test
  -- add positive self-loop edge
  Gr.updateEdgeFloydWarshall fw 1 (maxBound `div` 2) 0 0 1
  test
  -- add negative self-loop edge
  Gr.updateEdgeFloydWarshall fw 1 (maxBound `div` 2) 0 0 (-1)
  {- TODO: write test here -}
  pure ()

tests :: [TestTree]
tests =
  [ QC.testProperty "topSort" prop_topSort,
    -- not writing much tests, as we have verification problems
    QC.testProperty "floydWarshall" prop_floydWarshall,
    unit_one
  ]
