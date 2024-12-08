module Main where

import Bench.MulMod qualified
import Bench.PowMod qualified
import Criterion.Main

-- TODO: try tasty-bench

main :: IO ()
main =
  defaultMain
    [ Bench.MulMod.benches,
      Bench.PowMod.benches
    ]
