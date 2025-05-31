{-# LANGUAGE RecordWildCards #-}

-- | Not complete!
module Tests.Extra.Tree.Lct where

import AtCoder.Extra.Graph qualified as Gr
import AtCoder.Extra.Tree qualified as Tree
import AtCoder.Extra.Tree.Lct (Lct (..))
import AtCoder.Extra.Tree.Lct qualified as Lct
import AtCoder.Internal.Buffer qualified as B
import Control.Monad (when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Foldable (for_)
import Data.Maybe (isJust)
import Data.Semigroup (Sum (..))
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Test.QuickCheck.Monadic as QCM
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Tests.Util

-- | Maximum number of vertices.
maxN :: Int
maxN = 16

-- | Maximum number of queries.
maxQ :: Int
maxQ = 1000

-- | Range of monoid value.
rng :: (Int, Int)
rng = (-rngI, rngI)

-- | Range of monoid value.
rngI :: Int
rngI = 10

data Init s = Init
  { n :: {-# UNPACK #-} !Int,
    q :: {-# UNPACK #-} !Int,
    initialMonoids :: {-# UNPACK #-} !(VU.Vector (Sum Int)),
    refM :: !(IO Ref),
    lctM :: !(IO (Lct.Lct s (Sum Int)))
  }

-- | Referencial implementation
data Ref = Ref
  { nR :: {-# UNPACK #-} !Int,
    -- | Monoid values
    vecR :: !(VUM.MVector VUM.RealWorld (Sum Int)),
    -- | NxN
    edgeR :: !(VUM.MVector VUM.RealWorld Bool)
  }

newRef :: Int -> VU.Vector (Sum Int) -> IO Ref
newRef nR xs = do
  vecR <- VU.thaw xs
  edgeR <- VUM.replicate (nR * nR) False
  pure Ref {..}

instance Show (Init s) where
  show Init {..} = show (n, q, initialMonoids)

instance QC.Arbitrary (Init VUM.RealWorld) where
  arbitrary = do
    n <- QC.chooseInt (1, maxN)
    q <- QC.chooseInt (1, maxQ)
    initialMonoids <- (VU.fromList <$>) $ QC.vectorOf n (Sum <$> QC.chooseInt rng)
    let lctM = Lct.buildInv negate initialMonoids VU.empty
    let refM = let nR = n in newRef n initialMonoids
    pure Init {..}

data Query
  = Read Int
  | Write Int (Sum Int)
  | Modify Int (Sum Int)
  | ModifyM Int (Sum Int)
  | Link Int Int
  | Cut Int Int
  | Evert Int
  | Expose Int
  | Expose_ Int
  | -- | Root Int
    Same Int Int
  | -- TODO: Test parent!

    -- | Parent Int
    -- | Jump
    JumpMaybe Int Int Int
  | -- | Lca Int Int
    -- | LcaMaybe Int Int
    ProdPath Int Int
  | ProdSubtree Int Int
  -- -- | ProdTree Int Int
  deriving (Eq, Show)

-- | Arbitrary return type for the `Query` result.
data Result
  = None
  | I !Int
  | MI !(Maybe Int)
  | B Bool
  | SI (Sum Int)
  deriving (Show, Eq)

queryGen :: Int -> QC.Gen Query
queryGen n =
  QC.oneof
    [ Read <$> u,
      Write <$> u <*> x,
      Modify <$> u <*> x,
      ModifyM <$> u <*> x,
      Link <$> u <*> v,
      Cut <$> u <*> v,
      Evert <$> u,
      Expose <$> u,
      Expose_ <$> u,
      -- \| Root <$> Int
      Same <$> u <*> v,
      -- \| Parent <$> Int
      -- \| Jump
      JumpMaybe <$> u <*> v <*> k,
      -- Lca <$> u <*> v,
      -- LcaMaybe <$> u <*> v,
      ProdPath <$> u <*> v
      -- TODO: test prod subtree
      -- ProdSubtree <$> u <*> v
    ]
  where
    u = QC.chooseInt (0, n - 1)
    v = QC.chooseInt (0, n - 1)
    x = Sum <$> QC.chooseInt rng
    k = QC.chooseInt (-1, n)

toCsrM :: Ref -> IO (Gr.Csr ())
toCsrM Ref {..} = do
  es <- VU.unsafeFreeze edgeR
  let uvs = (`VU.mapMaybe` VU.generate (nR * nR) id) $ \i ->
        let (!u, !v) = i `divMod` nR
         in if u /= v && es VG.! (nR * u + v)
              then Just (u, v)
              else Nothing
  pure $ Gr.build' nR uvs

findPath :: Gr.Csr () -> Int -> Int -> Maybe (VU.Vector Int)
findPath gr v1 v2
  | ws VG.! v2 == -1 = Nothing
  | otherwise = Just $ Gr.constructPathFromRoot tree v2
  where
    (!ws, !tree) = Gr.trackingBfs (Gr.nCsr gr) grF (-1 :: Int) src
    grF = Gr.adj1 gr
    src = VU.singleton (v1, 0)

-- | containers. (referencial implementation)
handleRef :: Ref -> Query -> IO Result
handleRef ref@Ref {..} q = do
  case q of
    Read i -> do
      SI <$> VGM.read vecR i
    Write i x -> do
      VGM.write vecR i x
      pure None
    Modify i x -> do
      VGM.modify vecR (+ x) i
      pure None
    ModifyM i x -> do
      VGM.modifyM vecR (pure . (+ x)) i
      pure None
    Link u v -> do
      VGM.write edgeR (nR * u + v) True
      VGM.write edgeR (nR * v + u) True
      pure None
    Cut u v -> do
      VGM.write edgeR (nR * u + v) False
      VGM.write edgeR (nR * v + u) False
      pure None
    Evert _ -> do
      pure None
    Expose _ -> do
      pure None
    Expose_ _ -> do
      pure None
    -- \| Root Int
    Same u v -> do
      gr <- toCsrM ref
      pure . B . isJust $ findPath gr u v
    -- \| Parent Int
    -- \| Jump
    JumpMaybe u v k -> do
      gr <- toCsrM ref
      case findPath gr u v of
        Nothing -> pure $ MI Nothing
        Just path -> pure . MI $ path VG.!? k
    -- LcaMaybe u v -> do
    --   gr <- toCsrM ref
    --   -- FIXME: HLD cannot be used
    --   let connected = isJust $ findPath gr u v
    --   if connected
    --     then pure . MI . Just $ Hld.lca (Hld.new gr) u v
    --     else pure $ MI Nothing
    ProdPath u v -> do
      gr <- toCsrM ref
      ws <- VU.unsafeFreeze vecR
      case findPath gr u v of
        Nothing -> pure $ SI mempty
        Just path -> pure . SI $ VU.foldMap (ws VG.!) path
    ProdSubtree u v -> do
      -- TODO: fix this
      pure None

-- | ACL
handleAcl :: (PrimMonad m) => Lct (PrimState m) (Sum Int) -> Query -> m Result
handleAcl lct q = case q of
  Read i -> do
    SI <$> Lct.read lct i
  Write i x -> do
    Lct.write lct i x
    pure None
  Modify i x -> do
    Lct.modify lct (+ x) i
    pure None
  ModifyM i x -> do
    Lct.modifyM lct (pure . (+ x)) i
    pure None
  Link u v -> do
    -- TODO: detect if already connect
    Lct.link lct u v
    pure None
  Cut u v -> do
    Lct.cut lct u v
    pure None
  Evert i -> do
    Lct.evert lct i
    pure None
  Expose i -> do
    _ <- Lct.expose lct i
    pure None
  Expose_ i -> do
    Lct.expose_ lct i
    pure None
  -- \| Root Int
  Same u v -> do
    B <$> Lct.same lct u v
  -- \| Parent Int
  -- \| Jump
  JumpMaybe u v k -> do
    MI <$> Lct.jumpMaybe lct u v k
  -- LcaMaybe u v -> do
  --   MI <$> Lct.lcaMaybe lct u v
  ProdPath u v -> do
    SI <$> Lct.prodPath lct u v
  ProdSubtree u v -> do
    SI <$> Lct.prodSubtree lct u v

prop_randomTest :: Init VUM.RealWorld -> QC.Property
prop_randomTest Init {..} = QCM.monadicIO $ do
  lct <- QCM.run lctM
  ref <- QCM.run refM
  qs <- QCM.pick $ QC.vectorOf q (queryGen n)
  record <- QCM.run $ B.new q
  for_ qs $ \query -> do
    b <- QCM.run $ filterRefM ref query
    when b $ do
      QCM.run $ B.pushBack record $ VU.DoNotUnboxStrict query
      record' <- QCM.run $ B.unsafeFreeze record
      expected <- QCM.run $ handleRef ref query
      result <- QCM.run $ handleAcl lct query
      when (expected /= result) $ do
        -- show all the run queries on failure
        let s = show . V.map (\(VU.DoNotUnboxStrict x) -> x) $ VU.convert record'
        QCM.monitor $ QC.counterexample $ "actual queries in use: " ++ s
      QCM.assertWith (expected == result) $ show (query, expected, result)

-- | Filter invalid queries
filterRefM :: Ref -> Query -> IO Bool
filterRefM ref@Ref {..} q = case q of
  Read _ -> pure True
  Write _ _ -> pure True
  Modify _ _ -> pure True
  ModifyM _ _ -> pure True
  Link u v
    | u == v -> pure False
    | otherwise -> not <$> same u v
  Cut u v -> do
    es <- VU.unsafeFreeze edgeR
    pure $ es VG.! (nR * u + v)
  Evert _ -> pure True
  Expose _ -> pure True
  Expose_ _ -> pure True
  Same _ _ -> pure True
  JumpMaybe u v k -> same u v
  -- LcaMaybe u v -> same u v
  ProdPath u v -> same u v
  ProdSubtree u v -> same u v
  where
    same u v = do
      gr <- toCsrM ref
      let connected = isJust $ findPath gr u v
      pure connected

tests :: [TestTree]
tests =
  [ QC.testProperty "randomTest" prop_randomTest
  ]
