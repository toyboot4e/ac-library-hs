{-# LANGUAGE TypeFamilies #-}

-- | Extra module of pre-defined monoids and operators. `SegAct` and monoids in submodules are
-- re-exported.
module AtCoder.Extra.Monoid
  ( SegAct (..),
    Affine1 (..),
    Affine1Repr,
    RangeSet(..),
    RangeSetId(..),
  )
where

import AtCoder.Extra.Monoid.Affine1 (Affine1 (..), Affine1Repr)
import AtCoder.Extra.Monoid.RangeSet (RangeSet (..))
import AtCoder.Extra.Monoid.RangeSetId (RangeSetId (..))
import AtCoder.LazySegTree (SegAct (..))
