{-# LANGUAGE RecordWildCards #-}

-- | Aho–Corasick algorithm is a fast dictionary-matching algorithm.
--
-- ==== __Example__
--
-- >>> import AtCoder.Extra.AhoCorasick qualified as AC
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> let patterns = V.fromList [VU.fromList [0, 1], VU.fromList [0, 2], VU.fromList [2, 3]]
-- >>> let ac = AC.build 26 patterns
-- >>> AC.size ac
-- 6
-- >>> AC.retrieve ac (VU.singleton 2)
-- 4
--
-- @since 1.5.3.0
module AtCoder.Extra.AhoCorasick
  ( AhoCorasick (..),
    build,
    size,
    next,
    nextN,
    retrieve,
  )
where

import AtCoder.Internal.Queue qualified as Q
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Control.Monad.ST (runST)
import Data.Foldable (for_)
import Data.IntMap.Strict qualified as IM
import Data.Maybe (fromJust)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Mutable qualified as VM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)

-- | Aho–Corasick algorithm
--
-- @since 1.5.3.0
data AhoCorasick = AhoCorasick
  { -- | The number of nodes in the trie.
    --
    -- @since 1.5.3.0
    sizeAc :: {-# UNPACK #-} !Int,
    -- | Vertex -> (Char -> Vertex)
    --
    -- @since 1.5.3.0
    nextAc :: !(V.Vector (IM.IntMap Int)),
    -- | Links to parent vertex.
    --
    -- @since 1.5.3.0
    parentAc :: !(VU.Vector Int),
    -- | Links to the longest suffix vertex.
    --
    -- @since 1.5.3.0
    suffixAc :: !(VU.Vector Int)
  }

-- | \(O(\Gamma \sum_i |S_i|)\)
--
-- ==== Constraints
-- - \(|S_i| > 0\)
--
-- @since 1.5.3.0
{-# INLINE build #-}
build ::
  (HasCallStack) =>
  -- | Pattern strings.
  V.Vector (VU.Vector Int) ->
  -- | Aho–Corasick automaton based on a trie.
  AhoCorasick
build patterns
  | VG.null patterns =
      AhoCorasick
        1
        (V.singleton IM.empty)
        (VU.replicate 1 0)
        (VU.replicate 1 0)
  | otherwise =
      let (!nNodes, !next, !parent) = buildTrie patterns
          !suffix = runBfs nNodes next
       in AhoCorasick nNodes next parent suffix

-- | \(O(1)\) Returns the number of nodes in the trie.
--
-- @since 1.5.3.0
{-# INLINE size #-}
size :: (HasCallStack) => AhoCorasick -> Int
size = sizeAc

-- | \(O(|S_i|)\) Retrieves the next node to visit.
--
-- ==== Constraints
-- - The resulting string must be contained in the trie.
-- - \(n > 0\)
--
-- @since 1.5.3.0
{-# INLINE next #-}
next ::
  (HasCallStack) =>
  -- | The automaton.
  AhoCorasick ->
  -- | Node (root, empty node is @0@).
  Int ->
  -- | Character.
  Int ->
  -- | Next node.
  Int
next AhoCorasick {..} v c0 =
  let !c' = inner c0
      !v' = fromJust $ IM.lookup c' (nextAc VG.! v)
   in v'
  where
    inner c
      -- fallback to a suffix
      -- TODO: why suffixAc -> Char?
      | IM.notMember c (nextAc VG.! v) = inner $! suffixAc VG.! c
      | otherwise = c

-- | \(O(|S_i|)\) Applies `next` N times for a given input string.
--
-- ==== Constraints
-- - The resulting string must be contained in the trie.
--
-- @since 1.5.3.0
{-# INLINE nextN #-}
nextN ::
  (HasCallStack) =>
  -- | The automaton.
  AhoCorasick ->
  -- | Node.
  Int ->
  -- | String.
  VU.Vector Int ->
  -- | Resulting node.
  Int
nextN ac v cs = case VU.uncons cs of
  Just (!c, !cs') ->
    let !v' = next ac v c
     in nextN ac v' cs'
  Nothing -> v

-- | \(O(|S_i|)\) Retrieves the node index for a string.
--
-- ==== Constraints
-- - The resulting string must be contained in the trie.
--
-- @since 1.5.3.0
{-# INLINE retrieve #-}
retrieve ::
  (HasCallStack) =>
  -- | The automaton.
  AhoCorasick ->
  -- | String.
  VU.Vector Int ->
  -- | Resulting node.
  Int
retrieve ac = nextN ac 0

-- | \(O(\Gamma \sum_i |S_i|)\)
{-# INLINEABLE buildTrie #-}
buildTrie :: (HasCallStack) => V.Vector (VU.Vector Int) -> (Int, V.Vector (IM.IntMap Int), VU.Vector Int)
buildTrie patterns = runST $ do
  let !nMaxNodes = (1 +) . V.sum $ V.map VU.length patterns
  -- (Vertex, Char) -> Vertex
  nextVec <- VM.replicate nMaxNodes IM.empty
  parentVec <- VUM.replicate nMaxNodes (0 :: Int)
  nNodesVec <- VUM.replicate 1 (1 :: Int)

  VG.forM_ patterns $ \pat -> do
    VG.foldM'
      ( \ !u c -> do
          v0 <- IM.lookup c <$> VGM.read nextVec u
          case v0 of
            Nothing -> do
              v <- VGM.read nNodesVec 0
              VGM.write nNodesVec 0 $! v + 1
              VGM.modify nextVec (IM.insert c v) u
              VGM.write parentVec v u
              pure v
            Just v -> pure v
      )
      0
      pat

  !nNodes <- VGM.read nNodesVec 0
  !next <- VG.take nNodes <$> V.unsafeFreeze nextVec
  !parent <- VG.take nNodes <$> VU.unsafeFreeze parentVec
  pure (nNodes, next, parent)

-- | \(O(\Gamma \sum_i |S_i|)\)
{-# INLINEABLE runBfs #-}
runBfs :: (HasCallStack) => Int -> V.Vector (IM.IntMap Int) -> VU.Vector Int
runBfs nNodes next = VU.create $ do
  -- BFS
  suffixVec <- VUM.replicate nNodes (0 :: Int)
  que <- Q.new @_ @Int nNodes

  for_ (IM.elems (next VG.! 0)) $ \v -> do
    when (v /= -1) $ do
      Q.pushBack que v

  fix $ \popLoop -> do
    q <- Q.popFront que
    case q of
      Nothing -> pure ()
      Just u -> do
        -- visit neighbors
        for_ (IM.assocs (next VG.! u)) $ \(!c, !v) -> do
          Q.pushBack que v
          -- find the longest suffix to continue with `c`
          flip fix u $ \suffixLoop p -> do
            !suf <- VGM.read suffixVec p
            case IM.lookup c (next VG.! suf) of
              Just sufC -> do
                VGM.write suffixVec v sufC
              Nothing
                | suf /= 0 -> suffixLoop suf
                | otherwise -> pure ()
        popLoop

  pure suffixVec
