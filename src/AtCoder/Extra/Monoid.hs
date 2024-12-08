{-# LANGUAGE TypeFamilies #-}

-- | Extra module of pre-defined monoids and operators. `SegAct` and monoids in submodules are
-- re-exported.
module AtCoder.Extra.Monoid
  ( SegAct (..),
    Affine2d (..),
    Affine2dRepr,
    RangeSet(..),
    RangeSetId(..),
  )
where

import AtCoder.Extra.Monoid.Affine2d (Affine2d (..), Affine2dRepr)
import AtCoder.Extra.Monoid.RangeSet (RangeSet (..))
import AtCoder.Extra.Monoid.RangeSetId (RangeSetId (..))
import AtCoder.LazySegTree (SegAct (..))
