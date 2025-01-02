-- | A permutation represented by a vector, mainly for binary exponentiation.
--
-- The permutation is considered to be a left semigroup action: \(p_2 (p_1 x) = (p_2 \circ p_1) x\).
--
-- @since 1.1.0.0
module AtCoder.Extra.Semigroup.Permutation
  ( -- * Permutation
    Permutation (..),

    -- * Constructors
    new,
    unsafeNew,
    ident,
    zero,

    -- * Metadata
    length,
  )
where

import AtCoder.Internal.Assert qualified as ACIA
import Data.Vector.Generic qualified as VG
import Data.Vector.Unboxed qualified as VU
import GHC.Stack (HasCallStack)
import Prelude hiding (length)

-- | A permutation represented by a vector, mainly for binary exponentiation.
--
-- The permutation is considered to be a left semigroup action: \(p_2 (p_1 x) = (p_2 \circ p_1) x\).
--
-- @since 1.1.0.0
newtype Permutation = Permutation
  { unPermutation :: VU.Vector Int
  }
  deriving newtype
    ( -- | @since 1.1.0.0
      Eq,
      -- | @since 1.1.0.0
      Show
    )

-- | \(O(1)\) Creates a `Permutation` with boundary check.
--
-- @since 1.1.0.0
{-# INLINE new #-}
new :: (HasCallStack) => VU.Vector Int -> Permutation
new xs = Permutation xs
  where
    n = VU.length xs
    !_ = VU.foldl' (\() i -> let !_ = ACIA.runtimeAssert (-1 <= i && i < n) "AtCoder.Extra.Semigroup.Permutation.new: index boundary error" in ()) () xs

-- | \(O(1)\) Creates a `Permutation` without boundary check.
--
-- @since 1.1.0.0
{-# INLINE unsafeNew #-}
unsafeNew :: (HasCallStack) => VU.Vector Int -> Permutation
unsafeNew = Permutation

-- | \(O(1)\) Creates an identity `Permutation` of length \(n\).
--
-- @since 1.1.0.0
{-# INLINE ident #-}
ident :: Int -> Permutation
ident = Permutation . (`VU.generate` id)

-- | \(O(1)\) Creates a zero `Permutation` of length \(n\). It's similar to `ident`, but filled
-- with \(-1\) and invalidates corresponding slots on composition.
--
-- @since 1.1.0.0
{-# INLINE zero #-}
zero :: Int -> Permutation
zero n = Permutation $ VU.replicate n (-1)

-- | \(O(1)\) Returns the length of the internal vector.
--
-- @since 1.1.0.0
{-# INLINE length #-}
length :: (HasCallStack) => Permutation -> Int
length = VU.length . unPermutation

-- | @since 1.1.0.0
instance Semigroup Permutation where
  {-# INLINE (<>) #-}
  Permutation r2 <> Permutation r1 = Permutation $ VU.map f r1
    where
      !_ = ACIA.runtimeAssert (VU.length r2 == VU.length r1) "AtCoder.Extra.Semigroup.Permutation.(<>): legth mismatch"
      f (-1) = -1
      f i = VG.unsafeIndex r2 i