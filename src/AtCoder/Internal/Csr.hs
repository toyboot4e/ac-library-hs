{-# LANGUAGE RecordWildCards #-}

-- | Immutable Compresed Sparse Row.
--
-- ==== Example
-- >>> import AtCoder.Internal.Csr qualified as C
-- >>> let csr = build 3 $ VU.fromList @(Int, Int) [(0, 1), (0, 2), (0, 3), (1, 2), (2, 3)]
-- >>> csr `C.adj` 0
-- [1,2,3]
-- >>> csr `C.adj` 1
-- [2]
-- >>> csr `C.adj` 2
-- [3]
--
-- @since 1.0.0
module AtCoder.Internal.Csr
  ( -- * Compressed sparse row
    Csr,
    -- * Constructor
    build,
    -- * Accessors
    adj,
  )
where

import Control.Monad.ST (runST)
import Data.Foldable (for_)
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Base qualified as VU -- V_2
import Data.Vector.Unboxed.Mutable qualified as VUM
import GHC.Stack (HasCallStack)

-- | Comperssed Sparse Row representation of a graph.
--
-- @since 1.0.0
data Csr e = Csr
  { startCsr :: !(VU.Vector Int),
    elistCsr :: !(VU.Vector e)
  }
  deriving
    ( -- | @since 1.0.0
      Eq,
      -- | @since 1.0.0
      Show
    )

-- | \(O(n + m)\) Creates `Csr`.
--
-- @since 1.0.0
{-# INLINE build #-}
build :: (HasCallStack, VU.Unbox e) => Int -> VU.Vector (Int, e) -> Csr e
build n edges = runST $ do
  start <- VUM.replicate (n + 1) (0 :: Int)

  let (VU.V_2 _ froms _) = edges
  VU.forM_ froms $ \from -> do
    VGM.modify start (+ 1) (from + 1)

  for_ [1 .. n] $ \i -> do
    prev <- VGM.read start (i - 1)
    VGM.modify start (+ prev) i

  elist <- VUM.unsafeNew (VU.length edges)
  counter <- VUM.unsafeNew n
  VUM.unsafeCopy counter $ VUM.init start
  VU.forM_ edges $ \(!from, !e) -> do
    c <- VGM.read counter from
    VGM.write elist c e
    VGM.write counter from (c + 1)

  startCsr <- VU.unsafeFreeze start
  elistCsr <- VU.unsafeFreeze elist
  pure Csr {..}

-- | \(O(1)\) Returns adjacent vertices.
--
-- @since 1.0.0
{-# INLINE adj #-}
adj :: (HasCallStack, VU.Unbox e) => Csr e -> Int -> VU.Vector e
adj Csr {..} i =
  let il = startCsr VG.! i
      ir = startCsr VG.! (i + 1)
   in VU.slice il (ir - il) elistCsr
