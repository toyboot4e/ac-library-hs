module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Tests.DSU qualified
import Tests.FenwickTree qualified
import Tests.MaxFlow qualified

main :: IO ()
main =
  defaultMain
    . testGroup "toplevel"
    $ [ testGroup "Tests.MaxFlow" Tests.MaxFlow.tests,
        testGroup "Tests.DSU" Tests.DSU.tests,
        testGroup "Tests.FenwickTree" Tests.FenwickTree.tests
      ]
