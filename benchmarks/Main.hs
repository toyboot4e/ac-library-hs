{-# LANGUAGE CPP #-}

module Main where

import Bench.AddMod qualified
import Bench.Matrix qualified
import Bench.ModInt qualified
import Bench.MulMod qualified
import Bench.PowMod qualified
import Bench.SwapDupe qualified
import Criterion.Main

-- TODO: try tasty-bench
#define MOD 998244353

main :: IO ()
main =
  defaultMain
    -- TODO: generate criterion graph by benchmark group?
    [ Bench.MulMod.benches,
      Bench.ModInt.benches,
      Bench.AddMod.benches,
      Bench.PowMod.benches,
      Bench.Matrix.benches,
      Bench.SwapDupe.benches
    ]
