-- | Miscellaneous vector methods.
--
-- @since 1.2.2.0
module AtCoder.Extra.Vector
  ( argsort,
  )
where

import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Generic qualified as VG
import Data.Vector.Unboxed qualified as VU

-- | \(O(n \log n)\) Returns indices of the vector, stably sorted by their value.
--
-- ==== Example
-- >>> import Data.Vector.Algorithms.Intro qualified as VAI
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> argsort $ VU.fromList [0, 1, 0, 1, 0]
-- [0,2,4,1,3]
--
-- @since 1.2.3.0
{-# INLINEABLE argsort #-}
argsort :: (Ord a, VU.Unbox a) => VU.Vector a -> VU.Vector Int
argsort xs =
  VU.modify
    ( VAI.sortBy
        ( \i j ->
            ( compare (xs VG.! i) (xs VG.! j) <> compare i j
            )
        )
    )
    $ VU.generate (VU.length xs) id

-- TODO: maybe add lexicographic permutations, combinations, and subsequences.

