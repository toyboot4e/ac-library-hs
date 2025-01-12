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

    -- ** Range set
    RangeSet (..),
    RangeSetRepr,

    -- ** Rolling hash
    RollingHash,
  )
where

import AtCoder.Extra.Monoid.Affine1 (Affine1 (..), Affine1Repr)
import AtCoder.Extra.Monoid.Mat2x2 (Mat2x2 (..), Mat2x2Repr)
import AtCoder.Extra.Monoid.RangeAdd (RangeAdd (..))
import AtCoder.Extra.Monoid.RangeSet (RangeSet (..), RangeSetRepr)
import AtCoder.Extra.Monoid.RollingHash (RollingHash)
import AtCoder.Extra.Monoid.V2 (V2 (..), V2Repr)
import AtCoder.LazySegTree (SegAct (..))
