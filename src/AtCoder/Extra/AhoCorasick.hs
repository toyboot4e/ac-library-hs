-- | Aho–Corasick algorithm is a fast dictionary-matching (multi-pattern matching) algorithm.
--
-- ==== __Example__
--
-- >>> import AtCoder.Extra.AhoCorasick qualified as AC
-- >>> import Data.Vector.Unboxed qualified as VU
--
-- Pattern strings must be given as @V.Vector (VU.Vector Int)@:
--
-- >>> let patterns = V.fromList [VU.fromList [0, 1], VU.fromList [0, 2], VU.fromList [2, 3, 4]]
-- >>> let ac = AC.build patterns
-- >>> AC.size ac
-- 7
--
-- The automaton could be run manually with `next` or `nextN`:
--
-- >>> AC.nextN ac {- empty node -} 0 (VU.fromList [0, 2, 3])
-- 5
--
-- `match` returns a vector of @(endPos, patternId)@:
--
-- >>> --                         [.....) pattern 0
-- >>> --                               [.......) pattern2
-- >>> AC.match ac $ VU.fromList [0, 1, 2, 3, 4]
-- [(2,0),(5,2)]
--
-- If you need a vector of @(startPos, patternId)@, you must manually map the result:
--
-- >>> let f (!end, !patId) = (end - VU.length (patterns V.! patId), patId)
-- >>> --                                    [.....) pattern 0
-- >>> --                                          [.......) pattern2
-- >>> VU.map f . AC.match ac $ VU.fromList [0, 1, 2, 3, 4]
-- [(0,0),(2,2)]
--
-- Note that duplicate patterns are only counted once with `match`.
--
-- @since 1.5.3.0
module AtCoder.Extra.AhoCorasick
  ( AhoCorasick (..),
    build,
    size,
    next,
    nextN,
    match,
  )
where

-- TODO: Generalize with Hash + Unbox? Int-only implementation is faster though.

import AtCoder.Extra.Vector qualified as EV
import AtCoder.Internal.Queue qualified as Q
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Control.Monad.ST (runST)
import Data.Foldable (for_)
import Data.HashMap.Strict qualified as HM
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Mutable qualified as VM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)

-- | Aho–Corasick algorithm data.
--
-- @since 1.5.3.0
data AhoCorasick = AhoCorasick
  { -- | The number of nodes in the trie.
    --
    -- @since 1.5.3.0
    sizeAc :: {-# UNPACK #-} !Int,
    -- | A trie (-like directed graph) of input words: Vertex -> (Char -> Vertex).
    --
    -- @since 1.5.3.0
    trieAc :: !(V.Vector (HM.HashMap Int Int)),
    -- | Node data of links to parent vertex.
    --
    -- @since 1.5.3.0
    parentAc :: !(VU.Vector Int),
    -- | Node data that represents completed pattern string or nothing (@-1@).
    --
    -- @since 1.5.3.0
    patternAc :: !(VU.Vector Int),
    -- | Node data of links to the longest suffix vertex.
    --
    -- @since 1.5.3.0
    suffixAc :: !(VU.Vector Int),
    -- | Node data of links to the longest suffix pattern vertex.
    --
    -- @since 1.5.3.0
    outputAc :: !(VU.Vector Int)
  }

-- | \(O(\sum_i |S_i|)\)
--
-- ==== Constraints
-- - \(|S_i| > 0\)
--
-- @since 1.5.3.0
{-# INLINEABLE build #-}
build ::
  (HasCallStack) =>
  -- | Pattern strings.
  V.Vector (VU.Vector Int) ->
  -- | Aho–Corasick automaton based on a trie.
  AhoCorasick
build patterns
  | VG.null patterns =
      -- root only
      AhoCorasick
        1
        (V.singleton HM.empty)
        (VU.replicate 1 (-1))
        (VU.replicate 1 0)
        (VU.replicate 1 0)
        (VU.replicate 1 0)
  | otherwise =
      let (!nNodes, !patternMap, !trie, !parent) = buildTrie patterns
          (!suffix, !output) = runBfs nNodes trie patternMap
       in AhoCorasick nNodes trie parent patternMap suffix output

-- | \(O(1)\) Returns the number of nodes in the trie.
--
-- @since 1.5.3.0
{-# INLINE size #-}
size :: (HasCallStack) => AhoCorasick -> Int
size = sizeAc

-- | \(O(1)\) Retrieves the next node to visit.
--
-- @since 1.5.3.0
{-# INLINEABLE next #-}
-- TODO: benchmark INLINE
next ::
  (HasCallStack) =>
  -- | The automaton.
  AhoCorasick ->
  -- | Current node ID (empty node is @0@).
  Int ->
  -- | Character.
  Int ->
  -- | Next node ID.
  Int
next AhoCorasick {trieAc, suffixAc} v0 c = inner v0
  where
    inner v = case HM.lookup c (trieAc VG.! v) of
      Just end -> end
      Nothing
        -- no hope
        | v == 0 -> 0
        -- fallback to the longest match suffix
        | otherwise -> inner $! suffixAc VG.! v

-- | \(n\) Applies `next` N times for a given input string.
--
-- ==== Constraints
--
-- @since 1.5.3.0
{-# INLINE nextN #-}
nextN ::
  (HasCallStack) =>
  -- | The automaton.
  AhoCorasick ->
  -- | Current node.
  Int ->
  -- | String.
  VU.Vector Int ->
  -- | Resulting node.
  Int
nextN ac = VU.foldl' (next ac)

-- | \(O(|T|)\) Runs dictionary matching (multi-pattern matching) in linear time and returns a list
-- of @(endPos, patId)@, where @[endPos - patLen, endPos)@ corresponds to the interval of original
-- source slice.
--
-- Note that duplicate patterns are counted just once with one of them; if pattern A and B are the
-- same, their appearence is counted as either A or B.
--
-- @since 1.5.3.0
{-# INLINEABLE match #-}
match :: (HasCallStack) => AhoCorasick -> VU.Vector Int -> VU.Vector (Int, Int)
match ac@AhoCorasick {patternAc, outputAc} =
  EV.iconcatMap (\i v -> VU.unfoldr (f i) v) . VU.postscanl' (next ac) 0
  where
    f :: Int -> Int -> Maybe ((Int, Int), Int)
    f _ 0 = Nothing
    f i v = case patternAc VG.! v of
      -- NOTE: Do not perform early return, as the initial vertex can be non-pattern
      -1 -> f i (outputAc VG.! v)
      -- NOTE: Here we use `i + 1`, where [pos - patLen, pos) makes up a half-open interval.
      pat -> Just ((i + 1, pat), outputAc VG.! v)

-- | \(O(\sum_i |S_i| \Gamma)\)
{-# INLINEABLE buildTrie #-}
buildTrie :: (HasCallStack) => V.Vector (VU.Vector Int) -> (Int, VU.Vector Int, V.Vector (HM.HashMap Int Int), VU.Vector Int)
buildTrie patternStrings = runST $ do
  let !nMaxNodes = (1 +) . V.sum $ V.map VG.length patternStrings

  -- allocator
  nNodesVec <- VUM.replicate 1 (1 :: Int)

  -- components
  nextVec <- VM.replicate nMaxNodes HM.empty
  parentVec <- VUM.replicate nMaxNodes (0 :: Int)

  -- create a trie and collect pattern vertices
  patternVerts <-
    (VU.convert <$>) . V.forM patternStrings $
      VG.foldM'
        ( \ !u c -> do
            v0 <- HM.lookup c <$> VGM.read nextVec u
            case v0 of
              Nothing -> do
                -- allocate a new vertex index
                v <- VGM.read nNodesVec 0
                VGM.write nNodesVec 0 $! v + 1
                -- store the next vertex link
                VGM.modify nextVec (HM.insert c v) u
                -- fill the vertex information
                VGM.write parentVec v u
                pure v
              Just v -> do
                pure v
        )
        0

  !nNodes <- VGM.read nNodesVec 0

  let !pattern = VU.create $ do
        -- We could replace the following with VU.accumulate
        patVec <- VUM.replicate nNodes (-1 :: Int)
        VU.iforM_ patternVerts $ \iPattern v -> do
          VGM.write patVec v iPattern
        pure patVec
  !trie <- VG.take nNodes <$> V.unsafeFreeze nextVec
  !parent <- VG.take nNodes <$> VU.unsafeFreeze parentVec
  pure (nNodes, pattern, trie, parent)

-- | \(O(\sum_i |S_i| \Gamma)\)
{-# INLINEABLE runBfs #-}
runBfs :: (HasCallStack) => Int -> V.Vector (HM.HashMap Int Int) -> VU.Vector Int -> (VU.Vector Int, VU.Vector Int)
runBfs nNodes trie patternMap = runST $ do
  suffixVec <- VUM.replicate nNodes (0 :: Int)
  outputVec <- VUM.replicate nNodes (0 :: Int)

  que <- Q.new @_ @Int nNodes
  for_ (HM.elems (trie VG.! 0)) $ \v -> do
    Q.pushBack que v

  -- TODO: deduplicate with `next`
  let nextM c u = case HM.lookup c (trie VG.! u) of
        Just end -> pure end
        Nothing
          | u == 0 -> pure 0
          | otherwise -> do
              v <- VGM.read suffixVec u
              nextM c v

  fix $ \popLoop -> do
    q <- Q.popFront que
    case q of
      Nothing -> pure ()
      Just u -> do
        -- visit neighbors
        for_ (HM.toList (trie VG.! u)) $ \(!c, !v) -> do
          Q.pushBack que v

          -- find the longest suffix to continue with `c`
          !suffix <- nextM c =<< VGM.read suffixVec u
          VGM.write suffixVec v suffix

          -- find the longest suffix that matches to a pattern
          let suffixPattern = patternMap VG.! suffix
          output <-
            if suffixPattern /= -1
              then pure suffix
              else VGM.read outputVec suffix
          VGM.write outputVec v output

        -- loop
        popLoop

  (,) <$> VU.unsafeFreeze suffixVec <*> VU.unsafeFreeze outputVec
