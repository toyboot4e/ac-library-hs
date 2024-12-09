module Main (main) where

import Test.Tasty
import Tests.MulMod qualified as Tests.PowMod

main :: IO ()
main =
  defaultMain
    . testGroup "toplevel"
    $ [ testGroup "Tests.PowMod" Tests.PowMod.tests
      ]
