-- | Generic tree functions.
--
-- @since 1.1.0.0
module AtCoder.Extra.Graph.Tree
  ( fold,
    scan,
    foldReroot,
  )
where

import Data.Functor.Identity (runIdentity)
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM

{-# INLINE foldImpl #-}
foldImpl ::
  forall m w f a.
  (Monad m, VU.Unbox w) =>
  (Int -> VU.Vector (Int, w)) ->
  (Int -> a) ->
  (a -> (Int, w) -> f) ->
  (f -> a -> a) ->
  Int ->
  (Int -> a -> m ()) ->
  m a
foldImpl tree valAt toF act root memo = inner (-1) root
  where
    inner :: Int -> Int -> m a
    inner !parent !v1 = do
      let !acc0 = valAt v1
      let !v2s = VU.filter ((/= parent) . fst) $ tree v1
      !res <- VU.foldM' (\acc (!v2, !w) -> (`act` acc) . (`toF` (v1, w)) <$> inner v1 v2) acc0 v2s
      memo v1 res
      pure res

-- | \(O(n)\) Folds a tree from a root vertex, also known as tree DP.
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import AtCoder.Extra.Graph.Tree qualified as Tree
-- >>> import Data.Semigroup (Sum (..))
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let gr = Gr.build @(Sum Int) 5 . Gr.swapDupe $ VU.fromList [(2, 1, Sum 1), (1, 0, Sum 1), (2, 3, Sum 1), (3, 4, Sum 1)]
-- >>> type W = Sum Int -- edge weight
-- >>> type F = Sum Int -- action type
-- >>> type X = Sum Int -- vertex value
-- >>> :{
--  let res = Tree.fold (gr `Gr.adjW`) valAt toF act 2
--        where
--          valAt :: Int -> X
--          valAt _i = mempty @(Sum Int)
--          toF :: X -> (Int, W) -> F
--          toF x (!_i, !dx) = x + dx
--          act :: F -> X -> X
--          act dx x = dx + x
--   in getSum res
-- :}
-- 4
{-# INLINE fold #-}
fold ::
  (VU.Unbox w) =>
  -- | A graph as a function.
  (Int -> VU.Vector (Int, w)) ->
  -- | Assignment of initial vertex values.
  (Int -> a) ->
  -- | Converts a vertex value into an action onto a neighbor vertex.
  (a -> (Int, w) -> f) ->
  -- | Performs an action onto a vertex value.
  (f -> a -> a) ->
  -- | Root vertex.
  Int ->
  -- | Tree folding result from the root vertex.
  a
fold tree valAt toF act root = runIdentity $ do
  foldImpl tree valAt toF act root (\_ _ -> pure ())

-- | \(O(n)\) Folds a tree from a root vertex, also known as tree DP. The calculation process on
-- every vertex is recoreded.
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import AtCoder.Extra.Graph.Tree qualified as Tree
-- >>> import Data.Semigroup (Sum (..))
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let n = 5
-- >>> let gr = Gr.build @(Sum Int) n . Gr.swapDupe $ VU.fromList [(2, 1, Sum 1), (1, 0, Sum 1), (2, 3, Sum 1), (3, 4, Sum 1)]
-- >>> type W = Sum Int -- edge weight
-- >>> type F = Sum Int -- action type
-- >>> type X = Sum Int -- vertex value
-- >>> :{
--  let res = Tree.scan n (gr `Gr.adjW`) valAt toF act 2
--        where
--          valAt :: Int -> X
--          valAt _i = mempty @(Sum Int)
--          toF :: X -> (Int, W) -> F
--          toF x (!_i, !dx) = x + dx
--          act :: F -> X -> X
--          act dx x = dx + x
--   in VU.map getSum res
-- :}
-- [0,1,4,1,0]
{-# INLINE scan #-}
scan ::
  (VG.Vector v a, VU.Unbox w) =>
  -- | The number of vertices.
  Int ->
  -- | A graph as a function.
  (Int -> VU.Vector (Int, w)) ->
  -- | Assignment of initial vertex values.
  (Int -> a) ->
  -- | Converts a vertex value into an action onto a neighbor vertex.
  (a -> (Int, w) -> f) ->
  -- | Performs an action onto a vertex value.
  (f -> a -> a) ->
  -- | Root vertex.
  Int ->
  -- | Tree scanning result from a root vertex.
  v a
scan n tree acc0At toF act root = VG.create $ do
  dp <- VGM.unsafeNew n
  !_ <- foldImpl tree acc0At toF act root $ \v a -> do
    VGM.unsafeWrite dp v a
  pure dp

-- | \(O(n)\) Folds a tree from every vertex, using the rerooting technique.
--
-- ==== __Example__
-- >>> import AtCoder.Extra.Graph qualified as Gr
-- >>> import AtCoder.Extra.Graph.Tree qualified as Tree
-- >>> import Data.Semigroup (Sum (..))
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let n = 5
-- >>> let gr = Gr.build @(Sum Int) n . Gr.swapDupe $ VU.fromList [(2, 1, Sum 1), (1, 0, Sum 1), (2, 3, Sum 1), (3, 4, Sum 1)]
-- >>> type W = Sum Int -- edge weight
-- >>> type F = Sum Int -- action type
-- >>> type X = Sum Int -- vertex value
-- >>> :{
--  let res = Tree.foldReroot n (gr `Gr.adjW`) valAt toF act
--        where
--          valAt :: Int -> X
--          valAt _i = mempty @(Sum Int)
--          toF :: X -> (Int, W) -> F
--          toF x (!_i, !dx) = x + dx
--          act :: F -> X -> X
--          act dx x = dx + x
--   in VU.map getSum res
-- :}
-- [4,4,4,4,4]
{-# INLINE foldReroot #-}
foldReroot ::
  forall w f a.
  (VU.Unbox a, VU.Unbox f, Monoid f, VU.Unbox w) =>
  -- | The number of vertices.
  Int ->
  -- | A graph as a function.
  (Int -> VU.Vector (Int, w)) ->
  -- | Assignment of initial vertex values.
  (Int -> a) ->
  -- | Converts a vertex value into an action onto a neighbor vertex.
  (a -> (Int, w) -> f) ->
  -- | Performs an action onto a vertex value.
  (f -> a -> a) ->
  -- | Tree folding result from every vertex as a root.
  VU.Vector a
foldReroot n tree valAt toF act = VU.create $ do
  -- Calculate tree DP for every vertex as a root:
  !dp <- VUM.unsafeNew n
  let reroot parent parentF v1 = do
        -- TODO: when the operator is not commutative?
        let !children = VU.filter ((/= parent) . fst) $ tree v1
        let !fL = VU.scanl' (\ !f (!v2, !w) -> (f <>) . (`toF` (v1, w)) $ treeDp VG.! v2) f0 children
        let !fR = VU.scanr' (\(!v2, !w) !f -> (<> f) . (`toF` (v1, w)) $ treeDp VG.! v2) f0 children

        -- save
        let !x1 = (parentF <> VU.last fL) `act` valAt v1
        VGM.unsafeWrite dp v1 x1

        VU.iforM_ children $ \i2 (!v2, !w) -> do
          -- composited operator excluding @v2@:
          let !f1 = parentF <> (fL VG.! i2) <> (fR VG.! (i2 + 1))
          let !v1Acc = f1 `act` valAt v1
          let !f2 = toF v1Acc (v2, w)
          reroot v1 f2 v2

  reroot (-1 :: Int) f0 root0
  pure dp
  where
    !root0 = 0 :: Int
    !f0 = mempty @f
    !treeDp = scan n tree valAt toF act root0 :: VU.Vector a
