module Main where

import Bench.PowMod qualified
import Criterion.Main

-- TODO: try tasty-bench

main :: IO ()
main =
  defaultMain
    [ Bench.PowMod.benches
    ]
