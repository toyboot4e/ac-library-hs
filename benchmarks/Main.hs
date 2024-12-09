module Main where

import Bench.MulMod qualified
import Bench.PowMod qualified
import Criterion.Main

-- TODO: try tasty-bench

main :: IO ()
main =
  defaultMain
  -- TODO: generate graph by benchmark group?
    [ -- Bench.MulMod.benches32,
      -- Bench.MulMod.benches64,
      Bench.PowMod.benches
    ]
