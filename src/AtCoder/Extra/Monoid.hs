{-# LANGUAGE TypeFamilies #-}

-- | Extra module of pre-defined `SegAct` instances and helpful monoids.
--
-- @since 1.0.0.0
module AtCoder.Extra.Monoid
  ( -- * Re-exports

    -- | It's mainly a list. It is recommended to use specific submodules.

    -- ** SegAct
    SegAct (..),

    -- ** Affine1
    Affine1 (..),
    Affine1Repr,

    -- ** Mat2x2
    Mat2x2 (..),
    Mat2x2Repr,
    V2 (..),
    V2Repr,

    -- ** Range add
    RangeAdd (..),

    -- ** Range write
    RangeWrite (..),
    RangeWriteRepr,

    -- ** Rolling hash
    RollingHash,

    -- ** Top N
    Top2 (..),
    Top2Repr,
    Top3 (..),
    Top3Repr,
    Top4 (..),
    Top4Repr,
  )
where

import AtCoder.Extra.Monoid.Affine1 (Affine1 (..), Affine1Repr)
import AtCoder.Extra.Monoid.Mat2x2 (Mat2x2 (..), Mat2x2Repr)
import AtCoder.Extra.Monoid.RangeAdd (RangeAdd (..))
import AtCoder.Extra.Monoid.RangeWrite (RangeWrite (..), RangeWriteRepr)
import AtCoder.Extra.Monoid.RollingHash (RollingHash)
import AtCoder.Extra.Monoid.Top2 (Top2 (..), Top2Repr)
import AtCoder.Extra.Monoid.Top3 (Top3 (..), Top3Repr)
import AtCoder.Extra.Monoid.Top4 (Top4 (..), Top4Repr)
import AtCoder.Extra.Monoid.V2 (V2 (..), V2Repr)
import AtCoder.LazySegTree (SegAct (..))
