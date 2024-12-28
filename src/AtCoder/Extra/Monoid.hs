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

    -- * Range add
    RangeAdd (..),
    RangeAddId (..),

    -- * Range set
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
