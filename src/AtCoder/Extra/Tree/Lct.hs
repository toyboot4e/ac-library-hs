{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | Link/cut tree: forest with monoid values.
--
-- ==== __Example__
--
-- Create a link/cut tree of @Sum Int@ with inverse operator `negate`:
--
-- >>> import AtCoder.Extra.Tree.Lct qualified as Lct
-- >>> import Data.Semigroup (Sum (..))
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> -- 0--1--2
-- >>> --    +--3
-- >>> lct <- Lct.buildInv negate (VU.generate 4 Sum) $ VU.fromList [(0, 1), (1, 2), (1, 3)]
--
-- Monoid products can be calculated for paths or subtrees:
--
-- >>> Lct.prodPath lct 0 2
-- Sum {getSum = 3}
--
-- >>> Lct.prodSubtree lct 1 {- parent -} 2
-- Sum {getSum = 4}
--
-- `root` returns the current root vertex of the underlying tree, which is not easy to predict:
--
-- >>> Lct.root lct 3
-- 2
--
-- Set (`evert`) the root of the underlying tree to \(0\) and get the `lca` of vertices \(2\) and
-- \(3\):
--
-- >>> Lct.evert lct 0
-- >>> Lct.lca lct 2 3
-- 1
--
-- Similar to @Hld@, `Lct` allows various tree queries:
--
-- >>> Lct.parent lct 3
-- Just 1
--
-- >>> Lct.jump lct 2 3 2
-- 3
--
-- Edges can be dynamically added (`link`) or removed (`cut`):
--
-- >>> -- 0  1  2
-- >>> --    +--3
-- >>> Lct.cut lct 0 1
-- >>> Lct.cut lct 1 2
-- >>> VU.generateM 4 (Lct.root lct)
-- [0,1,2,1]
--
-- >>> -- +-----+
-- >>> -- 0  1  2
-- >>> --    +--3
-- >>> Lct.link lct 0 2
-- >>> VU.generateM 4 (Lct.root lct)
-- [2,1,2,1]
--
-- @since 1.1.1.0
module AtCoder.Extra.Tree.Lct
  ( -- Link/cut tree
    Lct (..),
    Vertex,

    -- * Constructors
    new,
    newInv,
    build,
    buildInv,

    -- * Modifications

    -- ** Write
    write,
    modify,
    modifyM,

    -- ** Link/cut
    link,
    cut,

    -- ** Evert/expose
    evert,
    expose,
    expose_,

    -- * Tree queries

    -- ** Root, parent, jump, LCA
    root,
    parent,
    jump,
    lca,

    -- ** Products
    prodPath,
    prodSubtree,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import Control.Monad (unless, when)
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Control.Monad.ST (ST)
import Data.Bit
import Data.Bits
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)

-- import GHC.Stack (HasCallStack)

-- | Alias of vertex type.
type Vertex = Int

{-# INLINE undefLct #-}
undefLct :: Vertex
undefLct = -1

{-# INLINE nullLct #-}
nullLct :: Vertex -> Bool
nullLct = (== -1)

-- We could optimize the with options, but

-- | Link/cut tree.
--
-- @since 1.1.1.0
data Lct s a = Lct
  { -- | The number of vertices.
    --
    -- @since 1.1.1.0
    nLct :: {-# UNPACK #-} !Int,
    -- | Decomposed node data storage: left children.
    --
    -- @since 1.1.1.0
    lLct :: !(VUM.MVector s Vertex),
    -- | Decomposed node data storage: right children.
    --
    -- @since 1.1.1.0
    rLct :: !(VUM.MVector s Vertex),
    -- | Decomposed node data storage: parents.
    --
    -- @since 1.1.1.0
    pLct :: !(VUM.MVector s Vertex),
    -- | Decomposed node data storage: subtree sizes.
    --
    -- @since 1.1.1.0
    sLct :: !(VUM.MVector s Int),
    -- | Decomposed node data storage: reverse flag.
    --
    -- @since 1.1.1.0
    revLct :: !(VUM.MVector s Bit),
    -- | Decomposed node data storage: monoid values.
    --
    -- @since 1.1.1.0
    vLct :: !(VUM.MVector s a),
    -- | Decomposed node data storage: monoid products.
    --
    -- @since 1.1.1.0
    prodLct :: !(VUM.MVector s a),
    -- | Decomposed node data storage: dual monod product (right fold). This is required for
    -- non-commutative monoids only.
    --
    -- @since 1.1.1.0
    dualProdLct :: !(VUM.MVector s a),
    -- | Decomposed node data storage: path-parent monoid product. This works for subtree product
    -- queries over commutative monoids only.
    --
    -- @since 1.1.1.0
    midLct :: !(VUM.MVector s a),
    -- | Decomposed node data storage: monoid product of subtree. This works for subtree product
    -- queries over commutative monoids only.
    --
    -- @since 1.1.1.0
    subtreeProdLct :: !(VUM.MVector s a),
    -- | Inverse operator of the monoid. This works for subtree product queries over commutative
    -- monoids only.
    --
    -- @since 1.1.1.0
    invOpLct :: !(a -> a)
  }

-- | \(O(n)\) Creates a link/cut tree with \(n\) vertices and no edges. This setup disables subtree
-- queries (`prodSubtree`).
--
-- @since 1.1.1.0
{-# INLINE new #-}
new :: (PrimMonad m, Monoid a, VU.Unbox a) => Int -> m (Lct (PrimState m) a)
new = newInv id

-- | \(O(n + m \log n)\) Creates a link/cut tree with an inverse operator, initial monoid values and
-- no edges. This setup enables subtree queries (`prodSubtree`).
--
-- @since 1.1.1.0
{-# INLINE newInv #-}
newInv :: (PrimMonad m, Monoid a, VU.Unbox a) => (a -> a) -> Int -> m (Lct (PrimState m) a)
newInv !invOpLct nLct = buildInv invOpLct (VU.replicate nLct mempty) VU.empty

-- | \(O(n + m \log n)\) Creates a link/cut tree of initial monoid values and initial edges. This
-- setup disables subtree queries (`prodSubtree`).
--
-- @since 1.1.1.0
{-# INLINE build #-}
build ::
  (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) =>
  -- | Vertex monoid values
  VU.Vector a ->
  -- | Edges
  VU.Vector (Vertex, Vertex) ->
  -- | Link/cut tree
  m (Lct (PrimState m) a)
build xs es = stToPrim $ buildInv id xs es

-- | \(O(n + m \log n)\) Creates a link/cut tree with an inverse operator, initial monoid values and
-- initial edges. This setup enables subtree queries (`prodSubtree`).
--
-- @since 1.1.1.0
{-# INLINE buildInv #-}
buildInv ::
  (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) =>
  -- | Inverse operator
  (a -> a) ->
  -- | Vertex monoid values
  VU.Vector a ->
  -- | Edges
  VU.Vector (Vertex, Vertex) ->
  -- | Link/cut tree
  m (Lct (PrimState m) a)
buildInv invOpLct xs es = stToPrim $ buildST invOpLct xs es

-- -------------------------------------------------------------------------------------------------
-- Write
-- -------------------------------------------------------------------------------------------------

-- | Amortized \(O(\log n)\). Writes the monoid value of a vertex.
--
-- @since 1.1.1.0
{-# INLINE write #-}
write :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Lct (PrimState m) a -> Vertex -> a -> m ()
write lct v x = stToPrim $ do
  -- make @v@ the new root of the underlying tree:
  evertST lct v
  VGM.unsafeWrite (vLct lct) v x
  where
    !_ = ACIA.checkIndex "AtCoder.Extra.Lct.write" v (nLct lct)

-- | Amortized \(O(\log n)\). Given a user function \(f\), modifies the monoid value of a vertex
-- \(v\).
--
-- @since 1.1.1.0
{-# INLINE modify #-}
modify :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Lct (PrimState m) a -> (a -> a) -> Vertex -> m ()
modify lct f v = stToPrim $ do
  -- make @v@ the new root of the underlying tree:
  evertST lct v
  VGM.unsafeModify (vLct lct) f v
  where
    !_ = ACIA.checkIndex "AtCoder.Extra.Lct.modify" v (nLct lct)

-- | Amortized \(O(\log n)\). Given a user function \(f\), modifies the monoid value of a vertex
-- \(v\).
--
-- @since 1.1.1.0
{-# INLINE modifyM #-}
modifyM :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Lct (PrimState m) a -> (a -> m a) -> Vertex -> m ()
modifyM lct f v = do
  -- make @v@ the new root of the underlying tree:
  stToPrim $ evertST lct v
  VGM.unsafeModifyM (vLct lct) f v
  where
    !_ = ACIA.checkIndex "AtCoder.Extra.Lct.modifyM" v (nLct lct)

-- -------------------------------------------------------------------------------------------------
-- Link/cut operations
-- -------------------------------------------------------------------------------------------------

-- | Amortized \(O(\log n)\). Creates an edge between \(c\) and \(p\). In the represented tree, the
-- \(p\) will be the parent of \(c\).
--
-- @since 1.1.1.0
{-# INLINE link #-}
link :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Lct (PrimState m) a -> Vertex -> Vertex -> m ()
link lct c p = stToPrim $ linkST lct c p
  where
    !_ = ACIA.checkIndex "AtCoder.Extra.Lct.link" c (nLct lct)
    !_ = ACIA.checkIndex "AtCoder.Extra.Lct.link" p (nLct lct)

-- | Amortized \(O(\log n)\). Deletes an edge between \(u\) and \(v\).
--
-- @since 1.1.1.0
{-# INLINE cut #-}
cut :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Lct (PrimState m) a -> Vertex -> Vertex -> m ()
cut lct u v = stToPrim $ cutST lct u v
  where
    !_ = ACIA.checkIndex "AtCoder.Extra.Lct.cut" u (nLct lct)
    !_ = ACIA.checkIndex "AtCoder.Extra.Lct.cut" v (nLct lct)

-- | Amortized \(O(\log n)\). Makes \(v\) a new root of the underlying tree.
--
-- @since 1.1.1.0
{-# INLINE evert #-}
evert :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Lct (PrimState m) a -> Vertex -> m ()
evert lct v = stToPrim $ evertST lct v
  where
    !_ = ACIA.checkIndex "AtCoder.Extra.Lct.evert" v (nLct lct)

-- | Amortized \(O(\log n)\). Makes \(v\) and the root to be in the same preferred path (auxiliary
-- tree). After the opeartion, \(v\) will be the new root and all the children will be detached from
-- the preferred path.
--
-- @since 1.1.1.0
{-# INLINE expose #-}
expose :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Lct (PrimState m) a -> Vertex -> m Vertex
expose lct v = stToPrim $ exposeST lct v
  where
    !_ = ACIA.checkIndex "AtCoder.Extra.Lct.expose_" v (nLct lct)

-- | Amortized \(O(\log n)\). `expose` with the return value discarded.
--
-- @since 1.1.1.0
{-# INLINE expose_ #-}
expose_ :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Lct (PrimState m) a -> Vertex -> m ()
expose_ lct v0 = stToPrim $ do
  _ <- exposeST lct v0
  pure ()
  where
    !_ = ACIA.checkIndex "AtCoder.Extra.Lct.expose_" v0 (nLct lct)

-- -------------------------------------------------------------------------------------------------
-- Jump, LCA
-- -------------------------------------------------------------------------------------------------

-- | \(O(\log n)\) Returns the root of the underlying tree. Two vertices in the same connected
-- component have the same root vertex.
--
-- @since 1.1.1.0
{-# INLINE root #-}
root :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Lct (PrimState m) a -> Int -> m Vertex
root lct c0 = stToPrim $ rootST lct c0

-- | \(O(\log n)\) Returns the parent vertex in the underlying tree.
--
-- @since 1.1.1.0
{-# INLINE parent #-}
parent :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Lct (PrimState m) a -> Int -> m (Maybe Vertex)
parent lct x = stToPrim $ parentST lct x

-- | \(O(\log n)\) Given a path between \(u\) and \(v\), returns the \(k\)-th vertex of the path.
--
-- ==== Constraints
-- - The \(k\)-th vertex must exist.
--
-- @since 1.1.1.0
{-# INLINE jump #-}
jump :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Lct (PrimState m) a -> Vertex -> Vertex -> Int -> m Vertex
jump lct u v k = stToPrim $ jumpST lct u v k

-- | \(O(\log n)\) Returns the LCA of \(u\) and \(v\). Because the root of the underlying tree changes
-- in almost every operation, one might want to use `evert` beforehand.
--
-- ==== Constraints
-- - \(u\) and \(v\) must be in the same connected component.
--
-- @since 1.1.1.0
{-# INLINE lca #-}
lca :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Lct (PrimState m) a -> Int -> Int -> m Vertex
lca lct u v = stToPrim $ do
  ru <- rootST lct u
  rv <- rootST lct v
  let !_ = ACIA.runtimeAssert (ru == rv) $ "AtCoder.Extra.Lct.lca: given two vertices in different connected components " ++ show (u, v)
  _ <- exposeST lct u
  exposeST lct v

-- -------------------------------------------------------------------------------------------------
-- Monoid product
-- -------------------------------------------------------------------------------------------------

-- | Amortized \(O(\log n)\). Folds a path between \(u\) and \(v\) (inclusive).
--
-- @since 1.1.1.0
{-# INLINE prodPath #-}
prodPath :: (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) => Lct (PrimState m) a -> Vertex -> Vertex -> m a
prodPath lct@Lct {prodLct} u v = stToPrim $ do
  -- make @u@ the root of the underlying tree
  evertST lct u
  -- make @v@ in the same preferred path as @u@
  _ <- exposeST lct v
  -- now that @v@ is at the root of the auxiliary tree, its aggregation value is the path folding:
  VGM.unsafeRead prodLct v
  where
    !_ = ACIA.checkIndex "AtCoder.Extra.Lct.prodPath" u (nLct lct)
    !_ = ACIA.checkIndex "AtCoder.Extra.Lct.prodPath" v (nLct lct)

-- | Amortized \(O(\log n)\). Fold the subtree under \(v\), considering \(p\) as the root-side
-- vertex. Or, if \(p\) equals \(v\), \(v\) will be the new root.
--
-- ==== Constraints
-- - The inverse operator has to be set on construction (`newInv` or `buildInv`).
--
-- @since 1.1.1.0
{-# INLINE prodSubtree #-}
prodSubtree ::
  (HasCallStack, PrimMonad m, Monoid a, VU.Unbox a) =>
  -- | Link/cut tree
  Lct (PrimState m) a ->
  -- | Vertex
  Vertex ->
  -- | Root or parent
  Vertex ->
  -- | Subtree's monoid product
  m a
prodSubtree lct v rootOrParent = stToPrim $ prodSubtreeST lct v rootOrParent

-- -------------------------------------------------------------------------------------------------
-- Internal
-- -------------------------------------------------------------------------------------------------

{-# INLINEABLE buildST #-}
buildST ::
  (HasCallStack, Monoid a, VU.Unbox a) =>
  -- | Inverse operator
  (a -> a) ->
  -- | Vertex monoid values
  VU.Vector a ->
  -- | Edges
  VU.Vector (Vertex, Vertex) ->
  -- | Link/cut tree
  ST s (Lct s a)
buildST invOpLct xs es = do
  lct <- do
    let !nLct = VU.length xs
    lLct <- VUM.replicate nLct undefLct
    rLct <- VUM.replicate nLct undefLct
    pLct <- VUM.replicate nLct undefLct
    sLct <- VUM.replicate nLct 0
    revLct <- VUM.replicate nLct (Bit False)
    vLct <- VU.thaw xs
    prodLct <- VUM.replicate nLct mempty
    dualProdLct <- VUM.replicate nLct mempty
    midLct <- VUM.replicate nLct mempty
    subtreeProdLct <- VUM.replicate nLct mempty
    pure Lct {..}
  VU.forM_ es $ \(!u, !v) -> do
    link lct u v
  pure lct

-- * Balancing

-- | \(O(1)\) Rotates up a non-root node.
{-# INLINEABLE rotateST #-}
rotateST :: (Monoid a, VU.Unbox a) => Lct s a -> Vertex -> ST s ()
rotateST lct@Lct {pLct, lLct, rLct} v = do
  p <- VGM.unsafeRead pLct v
  pp <- VGM.unsafeRead pLct p
  pl <- VGM.unsafeRead lLct p

  c <-
    if pl == v
      then do
        -- rotate right:
        --   p      v  <-- reference from `pp` is updated later
        --  /        \
        -- v    ->    p
        --  \        /
        --   c      c
        c <- VGM.unsafeExchange rLct v p
        VGM.unsafeWrite lLct p c
        pure c
      else do
        -- rotate left:
        -- p          v  <-- reference from `pp` is updated later
        --  \        /
        --   v  ->  p
        --  /        \
        -- c          c
        c <- VGM.unsafeExchange lLct v p
        VGM.unsafeWrite rLct p c
        pure c

  updateNodeST lct p
  updateNodeST lct v

  -- update the reference from `pp`:
  unless (nullLct pp) $ do
    ppl <- VGM.unsafeRead lLct pp
    if ppl == p
      then VGM.unsafeWrite lLct pp v
      else do
        ppr <- VGM.unsafeRead rLct pp
        if ppr == p
          then VGM.unsafeWrite rLct pp v
          else do
            -- overwrite the light (path-parent) pointer:
            changeLightST lct pp p v

  -- update parent pointers to `pp`: pp <-- v <-- p <-- c
  VGM.unsafeWrite pLct v pp
  VGM.unsafeWrite pLct p v
  unless (nullLct c) $ do
    VGM.unsafeWrite pLct c p

-- | Amortized \(O(\log n)\). Moves a node up to the root, performing self-balancing heuristic
-- called rotations.
{-# INLINEABLE splayST #-}
splayST :: (Monoid a, VU.Unbox a) => Lct s a -> Vertex -> ST s ()
splayST lct@Lct {pLct} c = do
  pushNodeST lct c
  let inner = do
        isRootC <- isRootNodeST lct c
        unless isRootC $ do
          p <- VGM.unsafeRead pLct c
          pp <- if nullLct p then pure undefLct else VGM.unsafeRead pLct p
          placeP <- nodePlaceST lct p
          if placeP == RootNodeLct
            then do
              pushNodeST lct p
              pushNodeST lct c
              rotateST lct c
            else do
              placeC <- nodePlaceST lct c
              pushNodeST lct pp
              pushNodeST lct p
              pushNodeST lct c
              if placeC == placeP
                then do
                  -- Rotate right twice:
                  --
                  --       pp       p         c
                  --      /        / \         \
                  --    p     ->  c   pp  ->    p
                  --   /                         \
                  -- c                            pp

                  -- Or rotate left twice:
                  --
                  --  pp             p            c
                  --   \            / \          /
                  --    p     ->  pp   c  ->    p
                  --     \                     /
                  --      c                   pp

                  rotateST lct p
                  rotateST lct c
                else do
                  --       pp         pp         c
                  --      /          /          | \
                  --    p     ->   c      ->   p   pp
                  --     \        /
                  --      c      p
                  rotateST lct c
                  rotateST lct c
          inner
  inner

-- * Node helpers

-- | \(O(1)\)
{-# INLINE isRootNodeST #-}
isRootNodeST :: Lct s a -> Vertex -> ST s Bool
isRootNodeST lct v = do
  (== RootNodeLct) <$> nodePlaceST lct v

-- TODO: return heavy/light notion
data NodePlaceLct = RootNodeLct | LeftNodeLct | RightNodeLct
  deriving (Eq)

-- | \(O(1)\)
{-# INLINEABLE nodePlaceST #-}
nodePlaceST :: Lct s a -> Vertex -> ST s NodePlaceLct
nodePlaceST Lct {lLct, rLct, pLct} v = do
  p <- VGM.unsafeRead pLct v
  if nullLct p
    then pure RootNodeLct
    else do
      pl <- VGM.unsafeRead lLct p
      if pl == v
        then pure LeftNodeLct
        else do
          pr <- VGM.unsafeRead rLct p
          if pr == v
            then pure RightNodeLct
            else pure RootNodeLct

-- -------------------------------------------------------------------------------------------------
-- Node operations
-- -------------------------------------------------------------------------------------------------

-- | \(O(1)\) Propgates the lazily propagated values on a node.
{-# INLINEABLE pushNodeST #-}
pushNodeST :: (VU.Unbox a) => Lct s a -> Vertex -> ST s ()
pushNodeST lct@Lct {lLct, rLct, revLct} v = do
  Bit b <- VGM.unsafeExchange revLct v (Bit False)
  when b $ do
    l <- VGM.unsafeRead lLct v
    r <- VGM.unsafeRead rLct v
    unless (nullLct l) $ reverseNodeST lct l
    unless (nullLct r) $ reverseNodeST lct r

-- | \(O(1)\)
{-# INLINEABLE reverseNodeST #-}
reverseNodeST :: (VU.Unbox a) => Lct s a -> Vertex -> ST s ()
reverseNodeST lct@Lct {revLct} i = do
  -- lazily propagate new reverse from the children, or cancel:
  VGM.unsafeModify revLct (xor (Bit True)) i
  -- swap
  swapLrNodeST lct i

-- | \(O(1)\) Reverses the left and the right children, lazily and recursively.
{-# INLINEABLE swapLrNodeST #-}
swapLrNodeST :: (VU.Unbox a) => Lct s a -> Vertex -> ST s ()
swapLrNodeST Lct {lLct, rLct, prodLct, dualProdLct} i = do
  -- swap chidlren
  VGM.unsafeModifyM lLct (VGM.unsafeExchange rLct i) i
  -- swap prodLct[i] and dualProdLct[i]
  VGM.unsafeModifyM prodLct (VGM.unsafeExchange dualProdLct i) i

-- | \(O(1)\) Recomputes the node size and the monoid product.
{-# INLINEABLE updateNodeST #-}
updateNodeST :: (Monoid a, VU.Unbox a) => Lct s a -> Vertex -> ST s ()
updateNodeST Lct {..} i = do
  l <- VGM.unsafeRead lLct i
  r <- VGM.unsafeRead rLct i
  v <- VGM.unsafeRead vLct i
  m <- VGM.unsafeRead midLct i

  (!size', !prod', !dualProd', !subtreeProd') <-
    if nullLct l
      then pure (1 :: Int, v, v, v <> m)
      else do
        lSize <- VGM.unsafeRead sLct l
        lProd <- VGM.unsafeRead prodLct l
        lDualProd <- VGM.unsafeRead dualProdLct l
        lSubtreeProd <- VGM.unsafeRead subtreeProdLct l
        pure (lSize + 1, lProd <> v, v <> lDualProd, lSubtreeProd <> v <> m)

  (!size'', !prod'', !dualProd'', !subtreeProd'') <-
    if nullLct r
      then pure (size', prod', dualProd', subtreeProd')
      else do
        rSize <- VGM.unsafeRead sLct r
        rProd <- VGM.unsafeRead prodLct r
        rDualProd <- VGM.unsafeRead dualProdLct r
        rSubtreeProd <- VGM.unsafeRead subtreeProdLct r
        pure (size' + rSize, prod' <> rProd, rDualProd <> dualProd', subtreeProd' <> rSubtreeProd)

  VGM.unsafeWrite sLct i size''
  VGM.unsafeWrite prodLct i prod''
  VGM.unsafeWrite dualProdLct i dualProd''
  VGM.unsafeWrite subtreeProdLct i subtreeProd''

-- | \(O(1)\) Called on adding a path-parent edge. This is for subtree folding.
{-# INLINEABLE addLightST #-}
addLightST :: (Semigroup a, VU.Unbox a) => Lct s a -> Vertex -> Vertex -> ST s ()
addLightST Lct {subtreeProdLct, midLct} p c = do
  newChild <- VGM.unsafeRead subtreeProdLct c
  VGM.unsafeModify midLct (newChild <>) p

-- | \(O(1)\) Called on changing a path-parent edge. This is for subtree folding.
{-# INLINEABLE changeLightST #-}
changeLightST :: Lct s a -> Vertex -> Vertex -> Vertex -> ST s ()
changeLightST _lct _u _v _p = do
  pure ()

-- | \(O(1)\) Called on erasing a path-parent edge. This is for subtree folding.
{-# INLINEABLE eraseLightST #-}
eraseLightST :: (Semigroup a, VU.Unbox a) => Lct s a -> Vertex -> Vertex -> ST s ()
eraseLightST Lct {subtreeProdLct, midLct, invOpLct} p c = do
  sub <- VGM.unsafeRead subtreeProdLct c
  let !sub' = invOpLct sub
  VGM.unsafeModify midLct (<> sub') p

-- * Link/cut

-- | Amortized \(O(\log n)\).
{-# INLINEABLE linkST #-}
linkST :: (Monoid a, VU.Unbox a) => Lct s a -> Vertex -> Vertex -> ST s ()
linkST lct@Lct {pLct, rLct} c p = do
  -- make @c@ the new root of the underlying tree
  evertST lct c
  -- remove right children of @p@.
  _ <- exposeST lct p
  pushNodeST lct p

  -- dbgM $ do
  --   cp <- VGM.unsafeRead pLct c
  --   let !_ = ACIA.runtimeAssert (nullLct cp) $ "cp must be null: " ++ show (c, cp)
  --   pr <- VGM.unsafeRead rLct p
  --   let !_ = ACIA.runtimeAssert (nullLct pr) $ "pr must be null: " ++ show (p, pr)
  --   pure ()

  -- connect with a heavy edge:
  VGM.unsafeWrite pLct c p
  VGM.unsafeWrite rLct p c
  updateNodeST lct p

{-# INLINEABLE cutST #-}
cutST :: (Monoid a, VU.Unbox a) => Lct s a -> Vertex -> Vertex -> ST s ()
cutST lct@Lct {pLct, lLct} u v = do
  -- make @u@ the new root of the underlying tree
  evertST lct u
  -- make @v@ in the same preferred path as the root
  _ <- exposeST lct v

  -- dbgM $ do
  --   -- @v@ does not have any right children. because @u@ and @v@ are neighbors, @vl@ is @u@.
  --   vp <- VGM.unsafeRead pLct v
  --   let !_ = ACIA.runtimeAssert (nullLct vp) "vp must be null"
  --   vl <- VGM.unsafeRead lLct v
  --   let !_ = ACIA.runtimeAssert (vl == u) "vl must be `u`"
  --   pure ()

  -- do
  --   -- @v@ does not have any right children. because @u@ and @v@ are neighbors, @vl@ is @u@.
  --   vp <- VGM.unsafeRead pLct v
  --   vl <- VGM.unsafeRead lLct v
  --   let !_ = if nullLct vp then () else error "vp must be null"
  --   let !_ = if vl == u then () else error "vl must be `u`"
  --   pure ()

  -- delete the heavy edge.
  -- vl <- VGM.unsafeRead lLct v
  -- VGM.unsafeWrite pLct vl undefLct
  VGM.unsafeWrite pLct u undefLct
  VGM.unsafeWrite lLct v undefLct
  updateNodeST lct v

-- | Amortized \(O(\log n)\). Makes \(v\) a new root of the underlying tree.
{-# INLINEABLE evertST #-}
evertST :: (Monoid a, VU.Unbox a) => Lct s a -> Vertex -> ST s ()
evertST lct v = do
  -- make @v@ be in the same preferred path as root. note that @v@ is at the root of the auxiliary tree.
  _ <- exposeST lct v
  -- reverse all the edges with respect to @v@: make @v@ a new root of the auxiliary tree.
  reverseNodeST lct v
  pushNodeST lct v

{-# INLINEABLE exposeST #-}
exposeST :: (Monoid a, VU.Unbox a) => Lct s a -> Vertex -> ST s Vertex
exposeST lct@Lct {pLct, rLct} v0 = do
  let inner v lastRoot
        | nullLct v = pure lastRoot
        | otherwise = do
            -- go up to the top of the auxiliary tree:
            splayST lct v

            -- make @lastRoot@ the right child of @v@:
            --    v               v
            --   /|\        ->   /|\
            --    | r             | lastRoot  <-- @v0@ (in the @lastRoot@) will be connected to the root
            --    lastRoot        r
            r <- VGM.unsafeRead rLct v
            unless (nullLct r) $ addLightST lct v r
            unless (nullLct lastRoot) $ eraseLightST lct v lastRoot
            VGM.unsafeWrite rLct v lastRoot
            updateNodeST lct v

            -- go up to the next auxiliary tree:
            --    p
            --    |
            --    v
            --     \
            --      lastRoot
            vp <- VGM.unsafeRead pLct v
            inner vp v

  res <- inner v0 undefLct

  -- do
  --   -- FIXME: remove
  --   pRes <- VGM.unsafeRead pLct res
  --   unless (nullLct pRes) $ error $ "xxx must be null!!! " ++ show (res, pRes)

  splayST lct v0

  -- do
  --   -- FIXME: remove
  --   p <- VGM.unsafeRead pLct v0
  --   unless (nullLct p) $ error $ "must be null!!! " ++ show (res, v0, p)

  pure res

-- * Jump, LCA

{-# INLINEABLE rootST #-}
rootST :: (HasCallStack, Monoid a, VU.Unbox a) => Lct s a -> Int -> ST s Vertex
rootST lct@Lct {lLct} c0 = do
  _ <- exposeST lct c0
  pushNodeST lct c0
  let inner c = do
        cl <- VGM.unsafeRead lLct c
        if nullLct cl
          then pure c
          else do
            pushNodeST lct cl
            inner cl
  c' <- inner c0
  splayST lct c'
  pure c'
  where
    !_ = ACIA.checkIndex "AtCoder.Extra.Lct.rootST" c0 (nLct lct)

{-# INLINEABLE parentST #-}
parentST :: (HasCallStack, Monoid a, VU.Unbox a) => Lct s a -> Int -> ST s (Maybe Vertex)
parentST lct@Lct {lLct, rLct} x = do
  _ <- exposeST lct x
  pushNodeST lct x
  xl <- VGM.unsafeRead lLct x
  if nullLct xl
    then pure Nothing
    else do
      pushNodeST lct xl
      let inner y = do
            yr <- VGM.unsafeRead rLct y
            if nullLct yr
              then pure y
              else do
                pushNodeST lct yr
                inner yr
      Just <$> inner xl
  where
    !_ = ACIA.checkIndex "AtCoder.Extra.Lct.parentST" x (nLct lct)

{-# INLINEABLE jumpST #-}
jumpST :: (HasCallStack, Monoid a, VU.Unbox a) => Lct s a -> Vertex -> Vertex -> Int -> ST s Vertex
jumpST lct@Lct {lLct, rLct, sLct} u0 v0 k0 = do
  -- make @v0@ a new root of the underlying tree
  evertST lct v0
  -- make @u0@ in the same preferred path as the root (@v0)
  _ <- exposeST lct u0

  do
    size <- VGM.unsafeRead sLct u0
    let !_ = ACIA.runtimeAssert (0 <= k0 && k0 < size) "invalid jump"
    pure ()

  let inner k u = do
        pushNodeST lct u
        -- TODO: what is happening?
        ur <- VGM.unsafeRead rLct u
        urSize <- if nullLct ur then pure 0 else VGM.unsafeRead sLct ur
        case compare k urSize of
          LT -> inner k ur
          EQ -> pure u
          GT -> do
            ul <- VGM.unsafeRead lLct u
            inner (k - (urSize + 1)) ul

  res <- inner k0 u0
  splayST lct res
  pure res

{-# INLINEABLE prodSubtreeST #-}
prodSubtreeST ::
  (HasCallStack, Monoid a, VU.Unbox a) =>
  -- | Link/cut tree
  Lct s a ->
  -- | Vertex
  Vertex ->
  -- | Root or parent
  Vertex ->
  -- | Subtree's monoid product
  ST s a
prodSubtreeST lct@Lct {nLct, subtreeProdLct} v rootOrParent = do
  if v == rootOrParent
    then do
      -- `v` will be the root
      evertST lct v
      VGM.unsafeRead subtreeProdLct v
    else do
      -- @rootOrParent@ can be far. retrieve the adjacent vertex:
      parent_ <- jumpST lct v rootOrParent 1
      -- detach @v@ from the parent. now that it's the root of the subtree vertices, the aggregation
      -- value is the aggregation of all the subtree vertices.
      cutST lct v parent_
      res <- VGM.unsafeRead subtreeProdLct v
      -- attach again
      linkST lct v parent_
      pure res
  where
    !_ = ACIA.checkIndex "AtCoder.Extra.Lct.prodSubtree" v nLct
    !_ = ACIA.checkIndex "AtCoder.Extra.Lct.prodSubtree" rootOrParent nLct
