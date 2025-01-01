{-# LANGUAGE TypeFamilies #-}

-- | Extra module of pre-defined `SegAct` instances.
--
-- Be warned that they're not 100% guaranteed to be correct.
--
-- @since 1.0.0.0
module AtCoder.Extra.Monoid
  ( -- * SegAct (re-export)
    SegAct (..),

    -- * Affine1
    Affine1 (..),
    Affine1Repr,

    -- * Mat2x2
    Mat2x2 (..),
    Mat2x2Repr,
    V2 (..),
    V2Repr,

    -- * Range add
    RangeAdd (..),

    -- * Range set
    RangeSet (..),
  )
where

import AtCoder.Extra.Monoid.Affine1 (Affine1 (..), Affine1Repr)
import AtCoder.Extra.Monoid.Mat2x2 (Mat2x2 (..), Mat2x2Repr)
import AtCoder.Extra.Monoid.RangeAdd (RangeAdd (..))
import AtCoder.Extra.Monoid.RangeSet (RangeSet (..))
import AtCoder.Extra.Monoid.V2 (V2 (..), V2Repr)
import AtCoder.LazySegTree (SegAct (..))
