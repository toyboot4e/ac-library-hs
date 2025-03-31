-- | Miscellaneous vector methods.
--
-- @since 1.2.2.0
module AtCoder.Extra.Vector
  ( argsort,
    unsafePermuteInPlace,
    unsafePermuteInPlaceST,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import Control.Monad (unless)
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Control.Monad.ST (ST)
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU

-- TODO: test `unsafePermuteInPlace`
-- TODO: is `unsafePermuteInPlace` fast enough as specialized one?

-- | \(O(n \log n)\) Returns indices of the vector, stably sorted by their value.
--
-- ==== Example
-- >>> import Data.Vector.Algorithms.Intro qualified as VAI
-- >>> import Data.Vector.Unboxed qualified as VU
-- >>> argsort $ VU.fromList [0, 1, 0, 1, 0]
-- [0,2,4,1,3]
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

-- | \(O(n)\) Applies a permutation to a mutable vector in-place.
--
-- ==== Constraints
-- - The index array must be a permutation (0-based).
{-# INLINE unsafePermuteInPlace #-}
unsafePermuteInPlace :: (PrimMonad m, VGM.MVector v a) => v (PrimState m) a -> VU.Vector Int -> m ()
unsafePermuteInPlace vec is = stToPrim $ unsafePermuteInPlaceST vec is

-- | \(O(n)\) Applies a permutation to a mutable vector in-place.
--
-- ==== Constraints
-- - The index array must be a permutation (0-based).
{-# INLINEABLE unsafePermuteInPlaceST #-}
unsafePermuteInPlaceST :: (VGM.MVector v a) => v s a -> VU.Vector Int -> ST s ()
unsafePermuteInPlaceST vec is = do
  let !_ = ACIA.runtimeAssert (VGM.length vec == VG.length is) "AtCoder.Extra.Vector.unsafePermuteInPlaceST: the length of the index array must be equal to the length of the permuted vector"
  let inner i lastX = do
        VGM.unsafeWrite vec i lastX
        unless (i == 0) $ do
          let i0' = VG.unsafeIndex is i
          lastX' <- VGM.unsafeRead vec i
          inner i0' lastX'

  let i0' = VG.unsafeIndex is 0
  x0' <- VGM.unsafeRead vec 0
  inner i0' x0'
