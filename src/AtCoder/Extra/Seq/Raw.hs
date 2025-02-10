{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | Base module for implementing dynamic sequences. It internaly uses a splay tree and user has to
-- track the root node change.
--
-- @since 1.2.0.0
module AtCoder.Extra.Seq.Raw
  ( -- * Seq
    Seq (..),

    -- * Constructors
    newST,
    resetST,
    newNodeST,
    newSeqST,
    freeNodeST,
    freeSubtreeST,

    -- * Metadata
    capacity,
    lengthST,

    -- * Merge/split
    mergeST,
    merge3ST,
    merge4ST,
    splitST,
    split3ST,
    split4ST,
    splitLrST,
    sliceST,

    -- * Read/write
    readST,
    readMaybeST,
    writeST,
    modifyST,
    exchangeST,

    -- * Products
    prodST,
    prodMaybeST,
    prodAllST,

    -- * Applications
    applyInST,
    applyToRootST,
    reverseST,

    -- * Insert/delete
    insertST,
    deleteST,
    deleteST_,
    detachST,

    -- * Balancing
    rotateST,
    splayST,
    splayKthST,

    -- * Bisection methods

    -- ** C++-like
    ilowerBoundST,
    ilowerBoundM,
    ilowerBoundProdST,
    ilowerBoundProdM,

    -- ** Splits
    isplitMaxRightST,
    isplitMaxRightM,
    isplitMaxRightProdST,
    isplitMaxRightProdM,

    -- ** Max right
    imaxRightST,
    imaxRightM,
    imaxRightProdST,
    imaxRightProdM,

    -- * Conversions
    freezeST,
  )
where

import AtCoder.Extra.Pool qualified as P
import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.LazySegTree (SegAct (..))
import Control.Monad (unless, when)
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Control.Monad.ST (ST)
import Data.Bit
import Data.Bits hiding (rotate)
import Data.Coerce (coerce)
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)
import Prelude hiding (seq)

-- | Storages of dynamic sequences of monoid values with monoid actions on them through the `SegAct`
-- instance.
--
-- @since 1.2.0.0
data Seq s f a = Seq
  { -- | The maximum number of elements.
    --
    -- @since 1.2.0.0
    nSeq :: {-# UNPACK #-} !Int,
    -- | `Pool` for free slot management.
    --
    -- @since 1.2.0.0
    poolSeq :: !(P.Pool s ()),
    -- | Decomposed node data storage: left children.
    --
    -- @since 1.2.0.0
    lSeq :: !(VUM.MVector s P.Index),
    -- | Decomposed node data storage: right children.
    --
    -- @since 1.2.0.0
    rSeq :: !(VUM.MVector s P.Index),
    -- | Decomposed node data storage: parents.
    --
    -- @since 1.2.0.0
    pSeq :: !(VUM.MVector s P.Index),
    -- | Decomposed node data storage: subtree sizes.
    --
    -- @since 1.2.0.0
    sSeq :: !(VUM.MVector s Int),
    -- | Decomposed node data storage: monoid values.
    --
    -- @since 1.2.0.0
    vSeq :: !(VUM.MVector s a),
    -- | Decomposed node data storage: monoid products.
    --
    -- @since 1.2.0.0
    prodSeq :: !(VUM.MVector s a),
    -- | Decomposed node data storage: reversed flag of children.
    --
    -- @since 1.2.0.0
    revSeq :: !(VUM.MVector s Bit),
    -- | Decomposed node data storage: lazily propagated monoid action. Use @()@ if you don't need
    -- monoid actions.
    --
    -- @since 1.2.0.0
    lazySeq :: !(VUM.MVector s f)
  }

-- | \(O(n)\) Creates a new `Seq` of length \(n\).
--
-- @since 1.2.0.0
{-# INLINEABLE newST #-}
newST :: (Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Int -> ST s (Seq s f a)
newST nSeq = do
  poolSeq <- P.new nSeq
  lSeq <- VUM.unsafeNew nSeq
  rSeq <- VUM.unsafeNew nSeq
  pSeq <- VUM.unsafeNew nSeq
  sSeq <- VUM.unsafeNew nSeq
  vSeq <- VUM.unsafeNew nSeq
  prodSeq <- VUM.unsafeNew nSeq
  revSeq <- VUM.unsafeNew nSeq
  lazySeq <- VUM.unsafeNew nSeq
  pure Seq {..}

-- | \(O(1)\) Clears the sequence storage.
--
-- @since 1.2.0.0
{-# INLINE resetST #-}
resetST :: Seq s f a -> ST s ()
resetST Seq {poolSeq} = stToPrim $ P.clear poolSeq

-- | \(O(1)\) Allocates a new sequence of length \(1\).
--
-- @since 1.2.0.0
{-# INLINEABLE newNodeST #-}
newNodeST :: (HasCallStack, Monoid f, VU.Unbox f, VU.Unbox a) => Seq s f a -> a -> ST s P.Index
newNodeST Seq {..} x = do
  i <- P.alloc poolSeq ()
  VGM.write lSeq (coerce i) P.undefIndex
  VGM.write rSeq (coerce i) P.undefIndex
  VGM.write pSeq (coerce i) P.undefIndex
  VGM.write sSeq (coerce i) 1
  VGM.write vSeq (coerce i) x
  VGM.write prodSeq (coerce i) x
  VGM.write revSeq (coerce i) $ Bit False
  VGM.write lazySeq (coerce i) mempty
  pure i

-- | \(O(n)\) Allocates a new sequence.
--
-- @since 1.2.0.0
{-# INLINEABLE newSeqST #-}
newSeqST :: (HasCallStack, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq s f a -> VU.Vector a -> ST s P.Index
newSeqST seq@Seq {..} !xs = do
  -- [l, r)
  let inner l r
        | l >= r = pure P.undefIndex
        | l + 1 == r = newNodeST seq $ xs VG.! l
        | otherwise = do
            let !m = (l + r) `div` 2
            rootL <- inner l m
            rootR <- inner (m + 1) r
            root <- newNodeST seq (xs VG.! m)
            unless (P.nullIndex rootL) $ do
              VGM.write lSeq (coerce root) rootL
              VGM.write pSeq (coerce rootL) root
            unless (P.nullIndex rootR) $ do
              VGM.write rSeq (coerce root) rootR
              VGM.write pSeq (coerce rootR) root
            updateNodeST seq root
            pure root
  inner 0 (VU.length xs)

-- | \(O(1)\) Frees a node.
--
-- @since 1.2.0.0
{-# INLINE freeNodeST #-}
freeNodeST :: Seq s v a -> P.Index -> ST s ()
freeNodeST Seq {poolSeq} = P.free poolSeq

-- | \(O(n)\) Frees a subtree.
--
-- @since 1.2.0.0
{-# INLINEABLE freeSubtreeST #-}
freeSubtreeST :: (VU.Unbox a) => Seq s f a -> P.Index -> ST s ()
freeSubtreeST Seq {lSeq, rSeq, poolSeq} c0
  | P.nullIndex c0 = pure ()
  | otherwise = do
      let inner c = do
            cl <- VGM.read lSeq (coerce c)
            unless (P.nullIndex cl) (inner cl)
            cr <- VGM.read rSeq (coerce c)
            unless (P.nullIndex cr) (inner cr)
      inner c0
      P.free poolSeq c0

-- -------------------------------------------------------------------------------------------------
-- Metadata
-- -------------------------------------------------------------------------------------------------

-- | \(O(1)\) Returns the capacity of the sequence storage.
--
-- @since 1.2.1.0
{-# INLINE capacity #-}
capacity :: Seq s f a -> Int
capacity = nSeq

-- | \(O(1)\) Returns the length of a sequence or a subtree.
--
-- @since 1.2.1.0
{-# INLINE lengthST #-}
lengthST :: Seq s f a -> P.Index -> ST s Int
lengthST Seq {..} i
  | P.nullIndex i = pure 0
  | otherwise = VGM.read sSeq (coerce i)

-- -------------------------------------------------------------------------------------------------
-- Merge/split
-- -------------------------------------------------------------------------------------------------

{-# INLINE assertRootST #-}
assertRootST :: (HasCallStack) => Seq s f a -> P.Index -> ST s ()
assertRootST Seq {pSeq} i = do
  p <- VGM.read pSeq (coerce i)
  let !_ = ACIA.runtimeAssert (P.nullIndex p) $ "AtCoder.Extra.Seq.Raw.assertRootST: not a root (node `" ++ show i ++ "`, parent `" ++ show p ++ "`)"
  pure ()

{-# INLINE assertRootOrNullST #-}
assertRootOrNullST :: (HasCallStack) => Seq s f a -> P.Index -> ST s ()
assertRootOrNullST Seq {pSeq} i
  | P.nullIndex i = pure ()
  | otherwise = do
      p <- VGM.read pSeq (coerce i)
      let !_ = ACIA.runtimeAssert (P.nullIndex p) $ "AtCoder.Extra.Seq.Raw.assertRootOrNullST: not a root (node `" ++ show i ++ "`, parent `" ++ show p ++ "`)"
      pure ()

-- | Amortized \(O(\log n)\). Merges two sequences \(l, r\) into one in the given order, ignoring
-- empty sequences.
--
-- ==== Constraints
-- - The vertices must be either null or a root.
--
-- @since 1.2.0.0
{-# INLINEABLE mergeST #-}
mergeST :: (HasCallStack, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq s f a -> P.Index -> P.Index -> ST s P.Index
mergeST seq@Seq {pSeq, lSeq} lRoot rRoot
  | P.nullIndex lRoot = pure rRoot
  | P.nullIndex rRoot = pure lRoot
  | otherwise = do
      assertRootST seq lRoot
      assertRootST seq rRoot
      rRoot' <- splayKthST seq rRoot 0
      VGM.write lSeq (coerce rRoot') lRoot
      VGM.write pSeq (coerce lRoot) rRoot'
      updateNodeST seq rRoot'
      pure rRoot'

-- | Amortized \(O(\log n)\). Merges three sequences \(l, m, r\) into one in the given order,
-- ignoring empty sequences.
--
-- ==== Constraints
-- - The vertices must be either null or a root.
--
-- @since 1.2.0.0
{-# INLINE merge3ST #-}
merge3ST :: (HasCallStack, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq s f a -> P.Index -> P.Index -> P.Index -> ST s P.Index
merge3ST seq a b c = do
  r' <- mergeST seq a b
  mergeST seq r' c

-- | Amortized \(O(\log n)\). Merges four sequences \(l, b, c, d, m, r\) into one in the given
-- order, ignoring empty sequences.
--
-- ==== Constraints
-- - The vertices must be either null or a root.
--
-- @since 1.2.0.0
{-# INLINE merge4ST #-}
merge4ST :: (HasCallStack, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq s f a -> P.Index -> P.Index -> P.Index -> P.Index -> ST s P.Index
merge4ST seq a b c d = do
  r' <- mergeST seq a b
  r'' <- mergeST seq r' c
  mergeST seq r'' d

-- | Amortized \(O(\log n)\). Splits a sequences into two: \([0, k), [k, n)\).
--
-- ==== Constraints
-- - The node must be null or a root.
-- - \(0 \le k \le n\).
--
-- @since 1.2.0.0
{-# INLINEABLE splitST #-}
splitST :: (HasCallStack, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq s f a -> P.Index -> Int -> ST s (P.Index, P.Index)
splitST seq@Seq {..} root k = do
  assertRootOrNullST seq root
  if k == 0
    then pure (P.undefIndex, root)
    else do
      size <- VGM.read sSeq $ coerce root
      if k == size
        then pure (root, P.undefIndex)
        else do
          root' <- splayKthST seq root (k - 1)
          r <- VGM.exchange rSeq (coerce root') P.undefIndex
          VGM.write pSeq (coerce r) P.undefIndex
          updateNodeST seq root'
          pure (root', r)

-- | Amortized \(O(\log n)\). Splits a sequences into three: \([0, l), [l, r), [r, n)\).
--
-- ==== Constraints
-- - The node must be null or a root.
-- - \(0 \le l \le r \le n\).
--
-- @since 1.2.0.0
{-# INLINE split3ST #-}
split3ST :: (HasCallStack, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq s f a -> P.Index -> Int -> Int -> ST s (P.Index, P.Index, P.Index)
split3ST seq root l r = do
  (!root', !c) <- splitST seq root r
  (!a, !b) <- splitST seq root' l
  pure (a, b, c)

-- | Amortized \(O(\log n)\). Splits a sequences into four: \([0, i), [i, j), [j, k), [k, n)\).
--
-- ==== Constraints
-- - The node must be null or a root.
-- - \(0 \le i \le j \le k \le n\).
--
-- @since 1.2.0.0
{-# INLINE split4ST #-}
split4ST :: (HasCallStack, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq s f a -> P.Index -> Int -> Int -> Int -> ST s (P.Index, P.Index, P.Index, P.Index)
split4ST seq root i j k = do
  (!root', !d) <- splitST seq root k
  (!root'', !c) <- splitST seq root' j
  (!a, !b) <- splitST seq root'' i
  pure (a, b, c, d)

-- | Amortized \(O(\log n)\). Splits a sequence into three: \([0, \mathrm{root}), \mathrm{root}, [\mathrm{root} + 1, n)\).
--
-- ==== Constraints
-- - The node must be a root.
--
-- @since 1.2.0.0
{-# INLINEABLE splitLrST #-}
splitLrST :: (HasCallStack, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq s f a -> P.Index -> ST s (P.Index, P.Index, P.Index)
splitLrST seq@Seq {..} root = do
  assertRootST seq root
  s <- do
    rootL <- VGM.read lSeq (coerce root)
    if P.nullIndex rootL
      then pure 0
      else VGM.read sSeq (coerce rootL)
  split3ST seq root s (s + 1)

-- | Amortized \(O(\log n)\). Captures the root of a subtree of \([l, r)\). Splay the new root after
-- call.
--
-- ==== Constraints
-- - \(0 \le \lt r \le n\). Note that the interval must have positive length.
--
-- @since 1.2.0.0
{-# INLINEABLE sliceST #-}
sliceST :: (HasCallStack, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq s f a -> P.Index -> Int -> Int -> ST s P.Index
sliceST seq@Seq {..} root l r
  | l == 0 = do
      size <- VGM.read sSeq (coerce root)
      if r == size
        then pure root
        else do
          root' <- splayKthST seq root r
          VGM.read lSeq $ coerce root'
  | otherwise = do
      size <- VGM.read sSeq $ coerce root
      if r == size
        then do
          root' <- splayKthST seq root (l - 1)
          VGM.read rSeq $ coerce root'
        else do
          -- o--l--o--o--r--o
          --    [        )
          --             * root' (splayed)
          --          * rootL (detached from the root)
          -- \* rootL' (splayed)
          --    * right(rootL'): node that corresponds to [l, r)
          root' <- splayKthST seq root r
          rootL <- VGM.read lSeq $ coerce root'
          -- detach `rootL` from `root'`
          VGM.write pSeq (coerce rootL) P.undefIndex
          rootL' <- splayKthST seq rootL (l - 1)
          -- re-attach `rootL'` to `root'`
          VGM.write pSeq (coerce rootL') root'
          VGM.write lSeq (coerce root') rootL'
          updateNodeST seq root'
          VGM.read rSeq $ coerce rootL'

-- -------------------------------------------------------------------------------------------------
-- Modifications
-- -------------------------------------------------------------------------------------------------

-- | Amortized \(O(\log n)\). Reads the \(k\)-th node's monoid value.
--
-- ==== Constraints
-- - \(0 \le k \lt n\)
--
-- @since 1.2.0.0
{-# INLINEABLE readST #-}
readST :: (HasCallStack, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq s f a -> P.Index -> Int -> ST s (a, P.Index)
readST seq@Seq {..} root k = do
  assertRootST seq root
  root' <- splayKthST seq root k
  (,root') <$> VGM.read vSeq (coerce root')

-- | Amortized \(O(\log n)\). Reads the \(k\)-th node's monoid value.
--
-- ==== Constraints
-- - The root must be empty or a root.
--
-- @since 1.2.0.0
{-# INLINEABLE readMaybeST #-}
readMaybeST :: (HasCallStack, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq s f a -> P.Index -> Int -> ST s (Maybe (a, P.Index))
readMaybeST seq@Seq {..} root k
  | P.nullIndex root = pure Nothing
  | otherwise = do
      assertRootST seq root
      s <- VGM.read sSeq (coerce root)
      if 0 <= k && k < s
        then do
          root' <- splayKthST seq root k
          Just . (,root') <$> VGM.read vSeq (coerce root')
        else pure Nothing

-- | Amortized \(O(\log n)\). Writes to the \(k\)-th node's monoid value.
--
-- ==== Constraints
-- - The node must be a root.
-- - \(0 \le k \lt n\)
--
-- @since 1.2.0.0
{-# INLINEABLE writeST #-}
writeST :: (HasCallStack, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq s f a -> P.Index -> Int -> a -> ST s P.Index
writeST seq root k v = do
  assertRootST seq root
  root' <- splayKthST seq root k
  writeNodeST seq root' v
  pure root'

-- | Amortized \(O(\log n)\). Modifies the \(k\)-th node's monoid value.
--
-- ==== Constraints
-- - The node must be a root.
-- - \(0 \le k \lt n\)
--
-- @since 1.2.0.0
{-# INLINEABLE modifyST #-}
modifyST :: (HasCallStack, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq s f a -> P.Index -> (a -> a) -> Int -> ST s P.Index
modifyST seq root f k = do
  assertRootST seq root
  root' <- splayKthST seq root k
  modifyNodeST seq f root'
  pure root'

-- | Amortized \(O(\log n)\). Exchanges the \(k\)-th node's monoid value.
--
-- ==== Constraints
-- - The node must be a root.
-- - \(0 \le k \lt n\)
--
-- @since 1.2.0.0
{-# INLINEABLE exchangeST #-}
exchangeST :: (HasCallStack, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq s f a -> P.Index -> Int -> a -> ST s (a, P.Index)
exchangeST seq root k v = do
  assertRootST seq root
  root' <- splayKthST seq root k
  res <- exchangeNodeST seq root' v
  pure (res, root')

-- | Amortized \(O(\log n)\). Returns the monoid product in an interval \([l, r)\).
--
-- ==== Constraints
-- - The node must be a root
-- - \(0 \le l \le r \le n\)
--
-- @since 1.2.0.0
{-# INLINEABLE prodST #-}
prodST :: (HasCallStack, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq s f a -> P.Index -> Int -> Int -> ST s (a, P.Index)
prodST seq@Seq {sSeq} root l r = do
  s <- if P.nullIndex root then pure 0 else VGM.read sSeq (coerce root)
  let !_ = ACIA.checkInterval "AtCoder.Extra.Seq.Raw.prodST" l r s
  if l == r
    then pure (mempty, root)
    else unsafeProdST seq root l r

-- | Amortized \(O(\log n)\). Returns the monoid product in an interval \([l, r)\). Returns
-- `Nothing` if an invalid interval is given or for an empty sequence.
--
-- @since 1.2.0.0
{-# INLINEABLE prodMaybeST #-}
prodMaybeST :: (HasCallStack, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq s f a -> P.Index -> Int -> Int -> ST s (Maybe (a, P.Index))
prodMaybeST seq@Seq {sSeq} root l r
  | P.nullIndex root = pure Nothing
  | otherwise = do
      s <- VGM.read sSeq (coerce root)
      if not (ACIA.testInterval l r s)
        then pure Nothing
        else
          if l == r
            then pure $ Just (mempty, root)
            else Just <$> unsafeProdST seq root l r

-- | Amortized \(O(\log n)\).
--
-- ==== Constraint
-- - \(0 \le \lt r \le n\). Note that the interval must have positive length.
{-# INLINEABLE unsafeProdST #-}
unsafeProdST :: (HasCallStack, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq s f a -> P.Index -> Int -> Int -> ST s (a, P.Index)
unsafeProdST seq@Seq {..} root l r = do
  assertRootST seq root
  target <- sliceST seq root l r
  res <- VGM.read prodSeq $ coerce target
  splayST seq target True
  pure (res, target)

-- | Amortized \(O(\log n)\). Returns the monoid product of the whole sequence. Returns `mempty`
-- for an empty sequence.
--
-- ==== Constraint
-- - The node must be null or a root.
--
-- @since 1.2.0.0
{-# INLINEABLE prodAllST #-}
prodAllST :: (HasCallStack, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq s f a -> P.Index -> ST s a
prodAllST seq@Seq {..} root = do
  if P.nullIndex root
    then pure mempty
    else do
      assertRootST seq root
      VGM.read prodSeq $ coerce root

-- | Amortized \(O(\log n)\). Given an interval \([l, r)\), applies a monoid action \(f\).
--
-- ==== Constraints
-- - \(0 \le l \le r \le n\)
-- - The root must point to a non-empty sequence.
--
-- @since 1.2.0.0
{-# INLINEABLE applyInST #-}
applyInST :: (HasCallStack, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq s f a -> P.Index -> Int -> Int -> f -> ST s P.Index
applyInST seq@Seq {..} root l r act = do
  assertRootST seq root
  s <- if P.nullIndex root then pure 0 else VGM.read sSeq (coerce root)
  let !_ = ACIA.checkInterval "AtCoder.Extra.Seq.applyInST" l r s
  if l == r
    then pure root
    else do
      root' <- sliceST seq root l r
      applyNodeST seq root' act
      splayST seq root' True
      pure root'

-- | \(O(1)\) Applies a monoid action \(f\) to the root of a sequence.
--
-- @since 1.2.0.0
{-# INLINEABLE applyToRootST #-}
applyToRootST :: (HasCallStack, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq s f a -> P.Index -> f -> ST s ()
applyToRootST seq@Seq {..} root act
  | P.nullIndex root = pure ()
  | otherwise = do
      rootP <- VGM.read pSeq (coerce root)
      when (P.nullIndex rootP) $ do
        applyNodeST seq root act

-- | Amortized \(O(\log n)\). Reverses the sequence in \([l, r)\).
--
-- ==== Constraints
-- - The monoid action \(f\) must be commutative.
-- - The monoid value \(v\) must be commutative.
--
-- @since 1.2.0.0
{-# INLINEABLE reverseST #-}
reverseST :: (HasCallStack, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq s f a -> P.Index -> Int -> Int -> ST s P.Index
reverseST seq@Seq {sSeq} root0 l r
  | P.nullIndex root0 = pure P.undefIndex
  | otherwise = do
      s <- VGM.read sSeq (coerce root0)
      if not (ACIA.testInterval l r s)
        then pure root0
        else
          if l == r
            then pure root0
            else do
              root' <- sliceST seq root0 l r
              reverseNodeST seq root'
              splayST seq root' True
              pure root'

-- | Amortized \(O(\log n)\). Inserts a new node at \(k\) with initial monoid value \(v\). This
-- functions for an empty index.
--
-- ==== Constraints
-- - The node must be null or a root.
-- - \(0 \le k \le n\)
--
-- @since 1.2.0.0
{-# INLINEABLE insertST #-}
insertST :: (HasCallStack, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq s f a -> P.Index -> Int -> a -> ST s P.Index
insertST seq root k v = do
  if P.nullIndex root
    then do
      -- `insertST` is actually `insertOrNewNodeST`: it's specifically designed to work for an empty
      -- sequence.
      newNodeST seq v
    else do
      (!l, !r) <- splitST seq root k
      node <- newNodeST seq v
      merge3ST seq l node r

-- | Amortized \(O(\log n)\). Frees the \(k\)-th node and returns the monoid value of it.
--
-- ==== Constraints
-- - The node must be null or a root.
-- - \(0 \le k \lt n\)
--
-- @since 1.2.0.0
{-# INLINEABLE deleteST #-}
deleteST :: (HasCallStack, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq s f a -> P.Index -> Int -> ST s (a, P.Index)
deleteST seq@Seq {..} root i = do
  (!l, !m, !r) <- split3ST seq root i (i + 1)
  x <- VGM.read vSeq (coerce m)
  freeNodeST seq m
  root' <- mergeST seq l r
  pure (x, root')

-- | Amortized \(O(\log n)\). Frees the \(k\)-th node.
--
-- ==== Constraints
-- - The node must be null or a root.
-- - \(0 \le k \lt n\)
--
-- @since 1.2.0.0
{-# INLINEABLE deleteST_ #-}
deleteST_ :: (HasCallStack, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq s f a -> P.Index -> Int -> ST s P.Index
deleteST_ seq root i = do
  (!l, !m, !r) <- split3ST seq root i (i + 1)
  freeNodeST seq m
  root' <- mergeST seq l r
  pure root'

-- | Amortized \(O(\log n)\). Detaches the \(k\)-th node and returns the new root of the original
-- sequence.
--
-- ==== Constraints
-- - The node must be null or a root.
-- - \(0 \le k \lt n\)
--
-- @since 1.2.0.0
{-# INLINEABLE detachST #-}
detachST :: (HasCallStack, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq s f a -> P.Index -> Int -> ST s P.Index
detachST seq root i = do
  (!l, !m, !r) <- split3ST seq root i (i + 1)
  freeNodeST seq m
  root' <- mergeST seq l r
  pure root'

-- -------------------------------------------------------------------------------------------------
-- Balancing
-- -------------------------------------------------------------------------------------------------

-- | Amortized \(O(\log n)\). Rotates a child node.
--
-- ==== Constraints
-- - \(0 \le i \lt n\)
--
-- @since 1.2.0.0
{-# INLINEABLE rotateST #-}
rotateST :: (HasCallStack) => Seq s v a -> P.Index -> ST s ()
rotateST Seq {..} !i = do
  p <- VGM.read pSeq $ coerce i
  pl <- VGM.read lSeq $ coerce p

  c <-
    if pl == i
      then do
        --   p       i
        --  /         \
        -- i     ->    p
        --  \         /
        --   r       r
        r <- VGM.exchange rSeq (coerce i) p
        VGM.write lSeq (coerce p) r
        pure r
      else do
        -- p          i
        --  \        /
        --   i  ->  p
        --  /        \
        -- l          l
        l <- VGM.exchange lSeq (coerce i) p
        VGM.write rSeq (coerce p) l
        pure l

  pp <- VGM.read pSeq $ coerce p
  unless (P.nullIndex pp) $ do
    --   pp      pp
    --  /    -> /
    -- p       i
    VGM.modify lSeq (\ppl -> if ppl == p then i else ppl) $ coerce pp
    --   pp       pp
    --     \  ->    \
    --      p        i
    VGM.modify rSeq (\ppr -> if ppr == p then i else ppr) $ coerce pp

  -- set parents
  VGM.write pSeq (coerce i) pp
  VGM.write pSeq (coerce p) i
  unless (P.nullIndex c) $ do
    VGM.write pSeq (coerce c) p

-- | Amortized \(O(\log n)\). Moves up a node to be a root.
--
-- @since 1.2.0.0
{-# INLINEABLE splayST #-}
splayST :: (HasCallStack, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq s f a -> P.Index -> Bool -> ST s ()
splayST seq@Seq {..} i doneParentProp = do
  -- we have no chance to call `splayST` with `doneParentProp == False` though
  if doneParentProp
    then propNodeST seq i
    else propNodeFromRootST seq i

  let inner = do
        p <- VGM.read pSeq $ coerce i
        unless (P.nullIndex p) $ do
          pp <- VGM.read pSeq $ coerce p
          if P.nullIndex pp
            then do
              rotateST seq i
              updateNodeST seq p
              pure ()
            else do
              pl <- VGM.read lSeq $ coerce p
              pr <- VGM.read rSeq $ coerce p
              ppl <- VGM.read lSeq $ coerce pp
              ppr <- VGM.read rSeq $ coerce pp
              if pl == i && ppl == p || pr == i && ppr == p
                then do
                  -- same direction twice
                  rotateST seq p
                  rotateST seq i
                else do
                  rotateST seq i
                  rotateST seq i
              updateNodeST seq pp
              updateNodeST seq p
          inner

  inner
  updateNodeST seq i

-- | Amortized \(O(\log n)\). Finds \(k\)-th node and splays it. Returns the new root.
--
-- ==== Constraints
-- - \(0 \le k \lt n\)
--
-- @since 1.2.0.0
{-# INLINEABLE splayKthST #-}
splayKthST :: (HasCallStack, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq s f a -> P.Index -> Int -> ST s P.Index
splayKthST seq@Seq {..} root0 k0 = do
  size <- VGM.read sSeq $ coerce root0
  let !_ = ACIA.checkIndex "AtCoder.Extra.Seq.Raw.splayKthST" k0 size

  let inner root k = do
        propNodeST seq root
        l <- VGM.read lSeq $ coerce root
        -- The number of left children = the node's index counting from the leftmost.
        sizeL <- if P.nullIndex l then pure 0 else VGM.read sSeq $ coerce l
        case compare k sizeL of
          EQ -> pure root
          LT -> inner l k
          GT -> do
            r <- VGM.read rSeq $ coerce root
            inner r (k - (sizeL + 1))

  target <- inner root0 k0
  splayST seq target True
  pure target

-- -------------------------------------------------------------------------------------------------
-- Bisection methods
-- -------------------------------------------------------------------------------------------------

-- | Amortized \(O(\log n)\).
--
-- @since 1.2.0.0
{-# INLINE ilowerBoundST #-}
ilowerBoundST ::
  (SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Sequence storage
  Seq s f a ->
  -- | Root node
  P.Index ->
  -- | User predicate \(f(i, v_i)\) that takes the index and the monoid value
  (Int -> a -> Bool) ->
  -- | (r, root)
  ST s (Int, P.Index)
ilowerBoundST seq root f = stToPrim $ do
  if P.nullIndex root
    then pure (0, P.undefIndex)
    else do
      (!r, !_, !root') <- imaxRightST seq root f
      pure (r, root')

-- | Amortized \(O(\log n)\).
--
-- @since 1.2.0.0
{-# INLINE ilowerBoundM #-}
ilowerBoundM ::
  (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Sequence storage
  Seq (PrimState m) f a ->
  -- | Root node
  P.Index ->
  -- | User predicate \(f(i, v_i)\) that takes the index and the monoid value
  (Int -> a -> m Bool) ->
  -- | (r, root)
  m (Int, P.Index)
ilowerBoundM seq root f = do
  if P.nullIndex root
    then pure (0, P.undefIndex)
    else do
      (!r, !_, !root') <- imaxRightM seq root f
      pure (r, root')

-- | Amortized \(O(\log n)\).
--
-- ==== Constraints
-- - The node must be a root.
--
-- @since 1.2.0.0
{-# INLINE ilowerBoundProdST #-}
ilowerBoundProdST ::
  (SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Sequence storage
  Seq s f a ->
  -- | Root node
  P.Index ->
  -- | User predicate \(f(i, v_0 \dots v_i)\) that takes the index and the monoid product
  (Int -> a -> Bool) ->
  -- | (r, root)
  ST s (Int, P.Index)
ilowerBoundProdST seq root f = do
  if P.nullIndex root
    then pure (0, P.undefIndex)
    else do
      (!r, !_, !root') <- imaxRightProdST seq root f
      pure (r, root')

-- | Amortized \(O(\log n)\).
--
-- ==== Constraints
-- - The node must be a root.
--
-- @since 1.2.0.0
{-# INLINE ilowerBoundProdM #-}
ilowerBoundProdM ::
  (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Sequence storage
  Seq (PrimState m) f a ->
  -- | Root node
  P.Index ->
  -- | User predicate \(f(i, v_0 \dots v_i)\) that takes the index and the monoid product
  (Int -> a -> m Bool) ->
  -- | (r, root)
  m (Int, P.Index)
ilowerBoundProdM seq root f = do
  if P.nullIndex root
    then pure (0, P.undefIndex)
    else do
      (!r, !_, !root') <- imaxRightProdM seq root f
      pure (r, root')

-- | Amortized \(O(\log n)\). Given a monotonious sequence, returns the rightmost node \(v_k\)
-- where \(f(v)\) holds for every \([0, i) (0 \le i \lt k)\).
--
-- @since 1.2.0.0
{-# INLINE isplitMaxRightST #-}
isplitMaxRightST ::
  (SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Sequence storage
  Seq s f a ->
  -- | Root node
  P.Index ->
  -- | User predicate \(f(i, v_i)\) that takes the index and the monoid value
  (Int -> a -> Bool) ->
  -- | (left, right) sequences where \(f\) holds for the left
  ST s (P.Index, P.Index)
isplitMaxRightST seq root f = stToPrim $ isplitMaxRightM seq root (\i x -> pure (f i x))

-- | Amortized \(O(\log n)\). Given a monotonious sequence, returns the rightmost node \(v_k\)
-- where \(f(v)\) holds for every \([0, i) (0 \le i \lt k)\).
--
-- @since 1.2.0.0
{-# INLINEABLE isplitMaxRightM #-}
isplitMaxRightM ::
  (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Sequence storage
  Seq (PrimState m) f a ->
  -- | Root node
  P.Index ->
  -- | User predicate \(f(i, v_i)\) that takes the index and the monoid value
  (Int -> a -> m Bool) ->
  -- | (left, right) sequences where \(f\) holds for the left
  m (P.Index, P.Index)
isplitMaxRightM seq@Seq {..} root f
  | P.nullIndex root = pure (P.undefIndex, P.undefIndex)
  | otherwise = do
      stToPrim $ assertRootST seq root
      (!_, !c, !_) <- imaxRightM seq root f
      if P.nullIndex c
        then stToPrim $ do
          -- `f` does hot hold
          splayST seq root True
          pure (P.undefIndex, root)
        else stToPrim $ do
          splayST seq c True
          right <- VGM.read rSeq (coerce c)
          if P.nullIndex right
            then do
              -- `f` holds for the whole sequence
              pure (c, P.undefIndex)
            else do
              -- `f` holds for part of the sequence. detach the right child
              VGM.write pSeq (coerce right) P.undefIndex
              VGM.write rSeq (coerce c) P.undefIndex
              updateNodeST seq c
              pure (c, right)

-- | Amortized \(O(\log n)\). Given a monotonious sequence, returns the rightmost node \(v_k\)
-- where \(f(v)\) holds for every \([0, i) (0 \le i \lt k)\).
--
-- @since 1.2.0.0
{-# INLINE isplitMaxRightProdST #-}
isplitMaxRightProdST ::
  (SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Sequence storage
  Seq s f a ->
  -- | Root node
  P.Index ->
  -- | User predicate \(f(i, v_0 \dots v_i)\) that takes the index and the monoid value
  (Int -> a -> Bool) ->
  -- | (left, right) sequences where \(f\) holds for the left
  ST s (P.Index, P.Index)
isplitMaxRightProdST seq root f = stToPrim $ isplitMaxRightProdM seq root (\i x -> pure (f i x))

-- | Amortized \(O(\log n)\). Given a monotonious sequence, returns the rightmost node \(v_k\)
-- where \(f(v)\) holds for every \([0, i) (0 \le i \lt k)\).
--
-- @since 1.2.0.0
{-# INLINEABLE isplitMaxRightProdM #-}
isplitMaxRightProdM ::
  (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Sequence storage
  Seq (PrimState m) f a ->
  -- | Root node
  P.Index ->
  -- | User predicate \(f(i, v_i)\) that takes the index and the monoid value
  -- | \(r\)
  (Int -> a -> m Bool) ->
  -- | (left, right) sequences where \(f\) holds for the left
  m (P.Index, P.Index)
isplitMaxRightProdM seq@Seq {..} root f
  | P.nullIndex root = pure (P.undefIndex, P.undefIndex)
  | otherwise = do
      stToPrim $ assertRootST seq root
      (!_, !c, !_) <- imaxRightProdM seq root f
      if P.nullIndex c
        then stToPrim $ do
          -- `f` does hot hold
          splayST seq root True
          pure (P.undefIndex, root)
        else stToPrim $ do
          splayST seq c True
          right <- VGM.read rSeq (coerce c)
          if P.nullIndex right
            then do
              -- `f` holds for the whole sequence
              pure (c, P.undefIndex)
            else do
              -- `f` holds for part of the sequence. detach the right child
              VGM.write pSeq (coerce right) P.undefIndex
              VGM.write rSeq (coerce c) P.undefIndex
              updateNodeST seq c
              pure (c, right)

-- | Amortized \(O(\log n)\). Given a monotonious sequence, returns the rightmost node \(v\)
-- where \(f(v)\) holds for every \(v_i (0 \le i \lt k)\). Note that \(f\) works for a single
-- node, not a monoid product.
--
-- ==== Constraints
-- - The node must be a root.
--
-- @since 1.2.0.0
{-# INLINE imaxRightST #-}
imaxRightST ::
  (SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Sequence storage
  Seq s f a ->
  -- | Root node
  P.Index ->
  -- | User predicate \(f(i, v_i)\) that takes the index and the monoid value
  (Int -> a -> Bool) ->
  -- | (r, left, right)
  ST s (Int, P.Index, P.Index)
imaxRightST seq root0 f = stToPrim $ imaxRightM seq root0 (\i x -> pure (f i x))

-- | Amortized \(O(\log n)\). Given a monotonious sequence, returns the rightmost node \(v_k\)
-- where \(f(v)\) holds for every \(v_i (0 \le i \le k)\). Note that \(f\) works for a single
-- node, not a monoid product.
--
-- ==== Constraints
-- - The node must be a root.
--
-- @since 1.2.0.0
{-# INLINEABLE imaxRightM #-}
imaxRightM ::
  (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Sequence storage
  Seq (PrimState m) f a ->
  -- | Root node
  P.Index ->
  -- | User predicate \(f(i, v_i)\) that takes the index and the monoid value
  (Int -> a -> m Bool) ->
  -- | (r, left, right)
  m (Int, P.Index, P.Index)
imaxRightM seq@Seq {..} root0 f = do
  let inner offset parent root lastYes
        | P.nullIndex root = pure (offset, lastYes, parent)
        | otherwise = do
            stToPrim $ propNodeST seq root
            l <- stToPrim $ VGM.read lSeq (coerce root)
            v <- stToPrim $ VGM.read vSeq (coerce root)
            pos <- stToPrim $ do
              if P.nullIndex l
                then pure offset
                else (offset +) <$> VGM.read sSeq (coerce l)
            b <- f pos v
            if b
              then do
                r <- stToPrim $ VGM.read rSeq $ coerce root
                inner (pos + 1) root r root
              else do
                inner offset root l lastYes

  (!r, !yes, !root') <- inner 0 P.undefIndex root0 P.undefIndex
  stToPrim $ splayST seq root' True
  pure (r, yes, root')

-- | Amortized \(O(\log n)\). Given a monotonious sequence, returns the rightmost node \(v_k\)
-- where \(f(v)\) holds for every \([0, i) (0 \le i \lt k)\).
--
-- ==== Constraints
-- - The node must be a root.
--
-- @since 1.2.0.0
{-# INLINE imaxRightProdST #-}
imaxRightProdST ::
  (SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Sequence storage
  Seq s f a ->
  -- | Root node
  P.Index ->
  -- | User predicate \(f(i, v_0 \dots v_i)\) that takes the index and the monoid value
  (Int -> a -> Bool) ->
  -- | (ilowerBound, rightmost node, new root)
  ST s (Int, P.Index, P.Index)
imaxRightProdST seq root0 f = imaxRightProdM seq root0 (\i x -> pure (f i x))

-- | Amortized \(O(\log n)\). Given a monotonious sequence, returns the rightmost node \(v_k\)
-- where \(f(v)\) holds for every \([0, i) (0 \le i \lt k)\).
--
-- ==== Constraints
-- - The node must be a root.
--
-- @since 1.2.0.0
{-# INLINEABLE imaxRightProdM #-}
imaxRightProdM ::
  (PrimMonad m, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) =>
  -- | Sequence storage
  Seq (PrimState m) f a ->
  -- | Root node
  P.Index ->
  -- | User predicate \(f(i, v_0 \dots v_i)\) that takes the index and the monoid value
  (Int -> a -> m Bool) ->
  -- | (ilowerBound, rightmost node, new root)
  m (Int, P.Index, P.Index)
imaxRightProdM seq@Seq {..} root0 f = do
  let inner !acc offset parent root lastYes
        | P.nullIndex root = pure (offset, lastYes, parent)
        | otherwise = do
            stToPrim $ propNodeST seq root
            l <- stToPrim $ VGM.read lSeq $ coerce root
            pos <- stToPrim $ do
              if P.nullIndex l
                then pure offset
                else (offset +) <$> VGM.read sSeq (coerce l)
            -- [0, pos]
            prodM <- stToPrim $ do
              -- detach right child (temporarily) and read the product
              rootR <- VGM.exchange rSeq (coerce root) P.undefIndex
              updateNodeST seq root
              prodRoot <- VGM.read prodSeq (coerce root)
              -- attach the right child again
              VGM.write rSeq (coerce root) rootR
              updateNodeST seq root
              pure $! acc <> prodRoot
            b <- f pos prodM
            if b
              then do
                r <- stToPrim $ VGM.read rSeq $ coerce root
                inner prodM (pos + 1) root r root
              else do
                inner acc offset root l lastYes

  (!r, !yes, !root') <- inner mempty 0 P.undefIndex root0 P.undefIndex
  stToPrim $ splayST seq root' True
  pure (r, yes, root')

-- -------------------------------------------------------------------------------------------------
-- Conversions
-- -------------------------------------------------------------------------------------------------

-- | Amortized \(O(n)\). Returns the sequence of monoid values.
--
-- @since 1.2.0.0
{-# INLINE freezeST #-}
freezeST :: (HasCallStack, SegAct f a, Eq f, Monoid f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq s f a -> P.Index -> ST s (VU.Vector a)
freezeST seq@Seq {sSeq, lSeq, rSeq, vSeq} root0
  | P.nullIndex root0 = pure VU.empty
  | otherwise = do
      size <- VGM.read sSeq (coerce root0)
      res <- VUM.unsafeNew size
      let inner i root
            | P.nullIndex root = pure i
            | otherwise = do
                -- visit from left to right
                propNodeST seq root
                i' <- inner i =<< VGM.read lSeq (coerce root)
                vx <- VGM.read vSeq (coerce root)
                VGM.write res i' vx
                inner (i' + 1) =<< VGM.read rSeq (coerce root)
      _ <- inner 0 root0
      VU.unsafeFreeze res

-- -------------------------------------------------------------------------------------------------
-- Node methods
-- -------------------------------------------------------------------------------------------------

-- NOTE(pref): inlining these functions are important for the speed

-- | \(O(1)\) Recomputes the node size and the monoid product.
{-# INLINEABLE updateNodeST #-}
updateNodeST :: (Monoid a, VU.Unbox a) => Seq s f a -> P.Index -> ST s ()
updateNodeST Seq {..} i = do
  l <- VGM.read lSeq (coerce i)
  r <- VGM.read rSeq (coerce i)
  prodM <- VGM.read vSeq (coerce i)
  (!size', !prod') <-
    if P.nullIndex l
      then pure (1, prodM)
      else do
        sizeL <- VGM.read sSeq (coerce l)
        prodL <- VGM.read prodSeq (coerce l)
        pure (sizeL + 1, prodL <> prodM)
  (!size'', !prod'') <-
    if P.nullIndex r
      then pure (size', prod')
      else do
        sizeR <- VGM.read sSeq (coerce r)
        prodR <- VGM.read prodSeq (coerce r)
        pure (size' + sizeR, prod' <> prodR)
  VGM.write sSeq (coerce i) size''
  VGM.write prodSeq (coerce i) prod''

-- | \(O(1)\) Writes to the monoid value of a node.
{-# INLINE writeNodeST #-}
writeNodeST :: (Monoid a, VU.Unbox a) => Seq s f a -> P.Index -> a -> ST s ()
writeNodeST seq@Seq {..} root v = do
  assertRootST seq root
  VGM.write vSeq (coerce root) v
  updateNodeST seq root

-- | \(O(1)\) Modifies the monoid value of a node.
{-# INLINE modifyNodeST #-}
modifyNodeST :: (HasCallStack, Monoid a, VU.Unbox a) => Seq s f a -> (a -> a) -> P.Index -> ST s ()
modifyNodeST seq@Seq {..} f root = do
  assertRootST seq root
  VGM.modify vSeq f $ coerce root
  updateNodeST seq root

-- | \(O(1)\) Exchanges the monoid value of a node.
{-# INLINE exchangeNodeST #-}
exchangeNodeST :: (HasCallStack, Monoid a, VU.Unbox a) => Seq s f a -> P.Index -> a -> ST s a
exchangeNodeST seq@Seq {..} root v = do
  assertRootST seq root
  res <- VGM.exchange vSeq (coerce root) v
  updateNodeST seq root
  pure res

-- | \(O(1)\) Swaps the left and the right children.
{-# INLINE swapLrNodeST #-}
swapLrNodeST :: Seq s f a -> P.Index -> ST s ()
swapLrNodeST Seq {..} i = do
  VGM.modifyM lSeq (VGM.exchange rSeq (coerce i)) (coerce i)

-- | \(O(1)\) Reverses the left and the right children, lazily and recursively.
{-# INLINE reverseNodeST #-}
reverseNodeST :: Seq s f a -> P.Index -> ST s ()
reverseNodeST seq@Seq {..} i = do
  swapLrNodeST seq i
  -- lazily propagate new reverse or cancel:
  VGM.modify revSeq (xor (Bit True)) $ coerce i

-- | Amortized \(O(\log n)\). Propgates the lazily propagated values on a node.
{-# INLINE propNodeST #-}
-- NOTE(pref): Although this function is large, inlining it needs for the speed.
propNodeST :: (HasCallStack, SegAct f a, Eq f, VU.Unbox f, Monoid a, VU.Unbox a) => Seq s f a -> P.Index -> ST s ()
propNodeST seq@Seq {..} i = do
  -- action
  act <- VGM.exchange lazySeq (coerce i) mempty
  -- this is where `Eq f` is required:
  when (act /= mempty) $ do
    l <- VGM.read lSeq $ coerce i
    unless (P.nullIndex l) $ do
      applyNodeST seq l act
    r <- VGM.read rSeq $ coerce i
    unless (P.nullIndex r) $ do
      applyNodeST seq r act

  -- reverse
  Bit b <- VGM.exchange revSeq (coerce i) (Bit False)
  when b $ do
    l <- VGM.read lSeq $ coerce i
    unless (P.nullIndex l) $ do
      -- propagate new reverse or cancel:
      reverseNodeST seq l
    r <- VGM.read rSeq $ coerce i
    unless (P.nullIndex r) $ do
      -- propagate new reverse or cancel:
      reverseNodeST seq r

-- | Amortized \(O(\log n)\). Propagetes from the root to the given node.
{-# INLINE propNodeFromRootST #-}
propNodeFromRootST :: (HasCallStack, SegAct f a, VU.Unbox f, VU.Unbox a, Monoid a) => Seq s f a -> P.Index -> ST s ()
propNodeFromRootST Seq {..} i0 = inner i0
  where
    inner i = do
      p <- VGM.read pSeq $ coerce i
      unless (P.nullIndex p) $ do
        inner p
      inner i

-- | Amortized \(O(\log n)\). Propgates at a node.
{-# INLINE applyNodeST #-}
applyNodeST :: (HasCallStack, SegAct f a, VU.Unbox f, Monoid a, VU.Unbox a) => Seq s f a -> P.Index -> f -> ST s ()
applyNodeST Seq {..} i act = do
  len <- VGM.read sSeq $ coerce i
  VGM.modify vSeq (segAct act) $ coerce i
  VGM.modify prodSeq (segActWithLength len act) $ coerce i
  VGM.modify lazySeq (act <>) $ coerce i
