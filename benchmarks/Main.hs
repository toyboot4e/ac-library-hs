{-# LANGUAGE CPP #-}

module Main where

import Bench.AddMod qualified
import Bench.MulMod qualified
import Bench.PowMod qualified
import Criterion.Main

-- TODO: try tasty-bench
#define MOD 998244353

main :: IO ()
main =
  defaultMain
  -- TODO: generate graph by benchmark group?
    [ -- Bench.MulMod.benches32,
      -- Bench.MulMod.benches64,
      -- Bench.AddMod.benches
      Bench.PowMod.benches
    ]
