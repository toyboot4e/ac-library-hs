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
import Bench.Vector.ConcatMapM qualified
import Bench.Vector.IConcatMapM qualified
import Bench.Vector.MapAccumL qualified
import Criterion.Main

main :: IO ()
main =
  defaultMain
    -- TODO: Filter benchmark cases with command line arguments. Currently I'm filtering benchmarks by hand.
    [ Bench.MulMod.benches,
      Bench.ModInt.benches,
      Bench.AddMod.benches,
      Bench.PowMod.benches,
      Bench.Matrix.benches,
      Bench.Montgomery64.benches,
      Bench.RepeatWithIndex.benches,
      Bench.RepeatWithoutIndex.benches,
      Bench.SwapDupe.benches,
      Bench.Vector.ConcatMapM.benches,
      Bench.Vector.IConcatMapM.benches,
      Bench.Vector.MapAccumL.benches
    ]
