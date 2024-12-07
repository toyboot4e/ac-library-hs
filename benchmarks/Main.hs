module Main where

import Criterion.Main

-- TODO: try tasty-bench

main :: IO ()
main =
  defaultMain
    [ Bench.PowMod.benches
    ]
