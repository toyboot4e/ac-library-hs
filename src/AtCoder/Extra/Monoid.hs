{-# LANGUAGE TypeFamilies #-}

-- | Extra module of pre-defined monoids and operators. `SegAct` and monoids in submodules are
-- re-exported.
--
-- While these monoids are `SegAct` samples, be warned that they're not guaanteed to be correct.
module AtCoder.Extra.Monoid
  ( SegAct (..),
    Affine1 (..),
    Affine1Repr,
    RangeAdd (..),
    RangeAddId (..),
    RangeSet (..),
    RangeSetId (..),
  )
where

import AtCoder.Extra.Monoid.Affine1 (Affine1 (..), Affine1Repr)
import AtCoder.Extra.Monoid.RangeAdd (RangeAdd (..))
import AtCoder.Extra.Monoid.RangeAddId (RangeAddId (..))
import AtCoder.Extra.Monoid.RangeSet (RangeSet (..))
import AtCoder.Extra.Monoid.RangeSetId (RangeSetId (..))
import AtCoder.LazySegTree (SegAct (..))
