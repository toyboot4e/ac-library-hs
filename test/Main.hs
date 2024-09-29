module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Tests.MaxFlow qualified

main :: IO ()
main =
  defaultMain
    . testGroup "toplevel"
    $ [ testGroup "Tests.MaxFlow" Tests.MaxFlow.tests
      ]
