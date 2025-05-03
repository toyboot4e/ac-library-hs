{-# LANGUAGE CPP #-}

module Main where

import Bench.AddMod qualified
import Bench.Matrix qualified
import Bench.ModInt qualified
import Bench.Montgomery64 qualified
import Bench.MulMod qualified
import Bench.PowMod qualified
import Bench.RepeatWithIndex qualified
import Bench.RepeatWithoutIndex qualified
import Bench.SwapDupe qualified
import Criterion.Main

main :: IO ()
main =
  defaultMain
    -- TODO: generate criterion graph by benchmark group?
    [ Bench.MulMod.benches,
      Bench.ModInt.benches,
      Bench.AddMod.benches,
      Bench.PowMod.benches,
      Bench.Matrix.benches,
      Bench.Montgomery64.benches,
      Bench.RepeatWithIndex.benches,
      Bench.RepeatWithoutIndex.benches,
      Bench.SwapDupe.benches
    ]
