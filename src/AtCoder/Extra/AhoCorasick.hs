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

import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.Queue qualified as Q
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Control.Monad.ST (runST)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
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
    -- | (Vertex, Char) -> Vertex
    --
    -- @since 1.5.3.0
    nextAc :: !(V.Vector (VU.Vector Int)),
    -- |
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
-- - \(n > 0\)
--
-- @since 1.5.3.0
{-# INLINE build #-}
build ::
  (HasCallStack) =>
  -- | The number of characters \(n\).
  Int ->
  -- | Pattern strings.
  V.Vector (VU.Vector Int) ->
  -- | Aho–Corasick automaton based on a trie.
  AhoCorasick
build n patterns
  | VG.null patterns =
      AhoCorasick
        1
        (V.singleton (VU.replicate n (-1)))
        (VU.replicate 1 0)
        (VU.replicate 1 0)
  | otherwise =
      let (!nNodes, !next, !parent) = buildTrie n patterns
          !suffix = runBfs nNodes next
       in AhoCorasick nNodes next parent suffix
  where
    !_ = ACIA.runtimeAssert (n > 0) $ "AtCoder.Extra.AhoCorasick.build: n must be positive (n = `" ++ show n ++ "`)"

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
  -- | The number of character.
  Int ->
  -- | Next node.
  Int
next AhoCorasick {..} v c0 =
  let !c' = inner c0
      !v' = nextAc VG.! v VG.! c'
   in v'
  where
    inner c
      -- fallback to a suffix
      -- TODO: why suffixAc -> Char?
      | nextAc VG.! v VG.! c == -1 = inner $! suffixAc VG.! c
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
buildTrie :: (HasCallStack) => Int -> V.Vector (VU.Vector Int) -> (Int, V.Vector (VU.Vector Int), VU.Vector Int)
buildTrie n patterns = runST $ do
  let !nMaxNodes = (+ 1) . VG.sum $ VG.map VG.length patterns
  flatNextVec <- VUM.replicate (n * nMaxNodes) (-1)
  let !nextVec = V.unfoldrExactN nMaxNodes (VUM.splitAt n) flatNextVec
  parentVec <- VUM.replicate nMaxNodes (0 :: Int)
  nNodesVec <- VUM.replicate 1 (1 :: Int)

  VG.forM_ patterns $ \pat -> do
    VG.foldM'
      ( \ !u c -> do
          v0 <- VGM.read (nextVec VG.! u) c
          if v0 == -1
            then do
              v <- VGM.read nNodesVec 0
              VGM.write nNodesVec 0 $! v + 1
              VGM.write (nextVec VG.! u) c v
              VGM.write parentVec v u
              pure v
            else do
              pure v0
      )
      0
      pat

  !nNodes <- VGM.read nNodesVec 0
  !next <- V.unfoldrExactN nNodes (VG.splitAt n) <$> VU.unsafeFreeze (VGM.take (n * nNodes) flatNextVec)
  !parent <- VU.unsafeFreeze $ VGM.take nNodes parentVec
  pure (nNodes, next, parent)

-- | \(O(\Gamma \sum_i |S_i|)\)
{-# INLINEABLE runBfs #-}
runBfs :: (HasCallStack) => Int -> V.Vector (VU.Vector Int) -> VU.Vector Int
runBfs nNodes next = VU.create $ do
  -- BFS
  suffixVec <- VUM.replicate nNodes (0 :: Int)
  que <- Q.new @_ @Int nNodes

  VU.forM_ (next VG.! 0) $ \v -> do
    when (v /= -1) $ do
      Q.pushBack que v

  fix $ \popLoop -> do
    q <- Q.popFront que
    case q of
      Nothing -> pure ()
      Just u -> do
        -- visit neighbors
        VU.iforM_ (next VG.! u) $ \c v -> do
          when (v /= -1) $ do
            Q.pushBack que v
            -- find the longest suffix to continue with `c`
            flip fix u $ \suffixLoop p -> do
              !suf <- VGM.read suffixVec p
              let !sufC = next VG.! suf VG.! c
              if sufC /= -1
                then do
                  VGM.write suffixVec v sufC
                else do
                  when (suf /= 0) $ do
                    suffixLoop suf
        popLoop

  pure suffixVec
