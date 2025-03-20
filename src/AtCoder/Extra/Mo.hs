{-# LANGUAGE RecordWildCards #-}

-- @since 1.2.3.0
module AtCoder.Extra.Mo
  (
  )
where

import Control.Monad (unless, when)
import Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import Data.Bits
import Data.Foldable (for_)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
