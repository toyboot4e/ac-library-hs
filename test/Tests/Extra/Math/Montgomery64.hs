{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}

module Tests.Extra.Math.Montgomery64 (tests) where

import AtCoder.Extra.Math qualified as ACEM
import AtCoder.Extra.Math.Montgomery64 qualified as M
import Data.Bits
import Data.WideWord.Word128 (Word128 (..))
import Data.Word (Word64)
import Test.Tasty
import Test.Tasty.QuickCheck qualified as QC

to128 :: (Integral a) => a -> Word128
to128 = fromIntegral

mulMod :: Word64 -> Word64 -> Word64 -> Word64
mulMod m x y = word128Lo64 $! (to128 (x `mod` m) * to128 (y `mod` m)) `mod` to128 m

p :: Word64 -> Bool
p m = odd m && m <= bit 62

prop_mulMod :: QC.Positive Word64 -> Word64 -> Word64 -> QC.Property
prop_mulMod (QC.Positive m) x y =
  p m QC.==>
    let !mont = M.fromVal m
        !res = M.decode mont $ M.mulMod mont (M.encode mont x) (M.encode mont y)
        !expected = mulMod m x y
     in res QC.=== expected

prop_powMod :: QC.Positive Word64 -> Word64 -> QC.Positive Int -> QC.Property
prop_powMod (QC.Positive m) x (QC.Positive n) =
  p m QC.==>
    let !mont = M.fromVal m
        !res = M.decode mont $ M.powMod mont (M.encode mont x) n
        !expected = ACEM.power (mulMod m) n (x `mod` m)
     in res QC.=== expected

tests :: [TestTree]
tests =
  [ QC.testProperty "mulMod" prop_mulMod,
    QC.testProperty "powMod" prop_powMod
  ]
