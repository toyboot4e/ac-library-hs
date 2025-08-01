-- | Math tests.
module Tests.Math (tests) where

import AtCoder.Math qualified as AM
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Data.Foldable
import Data.List qualified as L
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Test.Tasty
import Test.Tasty.HUnit

floorSumNaive :: Int -> Int -> Int -> Int -> Int
floorSumNaive n m a b = sum [(a * i + b) `div` m | i <- [0 .. n - 1]]

unit_powMod :: TestTree
unit_powMod = testCase "powMod" $ do
  let naive x n modulo =
        let y = x `mod` modulo
            z0 = 1 `mod` modulo
         in L.foldl' (\z' _ -> z' * y `mod` modulo) z0 [0 .. n - 1]
  for_ [-100 .. 100] $ \a -> do
    for_ [0 .. 100] $ \b -> do
      for_ [1 .. 100] $ \c -> do
        naive a b c @=? AM.powMod a b c

unit_invBoundHand :: TestTree
unit_invBoundHand = testCase "invBoundHand" $ do
  let min_ = minBound @Int
  let max_ = maxBound @Int
  AM.invMod (-1) max_ @=? AM.invMod min_ max_
  1 @=? AM.invMod max_ (max_ - 1)
  max_ - 1 @=? AM.invMod (max_ - 1) max_
  2 @=? AM.invMod (max_ `div` 2 + 1) max_

unit_invMod :: TestTree
unit_invMod = testCase "invMod" $ do
  for_ [-100 .. 100] $ \a -> do
    for_ [1 .. 1000] $ \b -> do
      when (gcd (a `mod` b) b == 1) $ do
        let c = AM.invMod a b
        assertBool "" $ 0 <= c
        assertBool "" $ c < b
        (1 `mod` b) @=? (a * c `mod` b + b) `mod` b

unit_invModZero :: TestTree
unit_invModZero = testCase "invModZero" $ do
  0 @=? AM.invMod 0 1
  for_ [0 .. 10 - 1] $ \i -> do
    0 @=? AM.invMod i 1
    0 @=? AM.invMod (-i) 1
    0 @=? AM.invMod (minBound @Int + i) 1
    0 @=? AM.invMod (maxBound @Int - i) 1

unit_floorSum :: TestTree
unit_floorSum = testCase "floorSum" $ do
  for_ [0 .. 20 - 1] $ \n -> do
    for_ [1 .. 20 - 1] $ \m -> do
      for_ [-20 .. 19] $ \a -> do
        for_ [-20 .. 19] $ \b -> do
          floorSumNaive n m a b @?= AM.floorSum n m a b

unit_crtHand :: TestTree
unit_crtHand = testCase "crtHand" $ do
  let (!res1, !res2) = AM.crt (VU.fromList [1, 2, 1]) (VU.fromList [2, 3, 2])
  5 @=? res1
  6 @=? res2

unit_crt2 :: TestTree
unit_crt2 = testCase "crt2" $ do
  for_ [1 .. 20] $ \a -> do
    for_ [1 .. 20] $ \b -> do
      for_ [-10 .. 10] $ \c -> do
        for_ [-10 .. 10] $ \d -> do
          let (!res1, !res2) = AM.crt (VU.fromList [c, d]) (VU.fromList [a, b])
          if res2 == 0
            then do
              for_ [0 .. a * b `div` gcd a b - 1] $ \x -> do
                assertBool "" $ x `mod` a /= c || x `mod` b /= d
            else do
              a * b `div` gcd a b @=? res2
              c `mod` a @=? res1 `mod` a
              d `mod` b @=? res1 `mod` b

unit_crt3 :: TestTree
unit_crt3 = testCase "crt3" $ do
  for_ [1 .. 5] $ \a -> do
    for_ [1 .. 5] $ \b -> do
      for_ [1 .. 5] $ \c -> do
        for_ [-5 .. 5] $ \d -> do
          for_ [-5 .. 5] $ \e -> do
            for_ [-5 .. 5] $ \f -> do
              let (!res1, !res2) = AM.crt (VU.fromList [d, e, f]) (VU.fromList [a, b, c])
              let lcm = a * b `div` gcd a b
              let lcm' = lcm * c `div` gcd lcm c
              if res2 == 0
                then do
                  for_ [0 .. lcm' - 1] $ \x -> do
                    assertBool "" $ x `mod` a /= d || x `mod` b /= e || x `mod` c /= f
                else do
                  lcm' @=? res2
                  d `mod` a @=? res1 `mod` a
                  e `mod` b @=? res1 `mod` b
                  f `mod` c @=? res1 `mod` c
              pure ()
  pure ()

unit_crtOverflow :: TestTree
unit_crtOverflow = testCase "crtOverflow" $ do
  let r0 = 0
  let r1 = 1_000_000_000_000 - 2
  let m0 = 900577
  let m1 = 1_000_000_000_000
  let (!res1, !res2) = AM.crt (VU.fromList [r0, r1]) (VU.fromList [m0, m1])
  m0 * m1 @=? res2
  r0 @=? res1 `mod` m0
  r1 @=? res1 `mod` m1

unit_crtBound :: TestTree
unit_crtBound = testCase "crtBound" $ do
  let inf = maxBound @Int
  let ps = VU.create $ do
        p <- VUM.unsafeNew (2 * 10 + 3)
        for_ [1 .. 10] $ \i -> do
          VGM.write p (2 * (i - 1) + 0) i
          VGM.write p (2 * (i - 1) + 1) $ inf - (i - 1)
        VGM.write p (2 * 10 + 0) 998244353
        VGM.write p (2 * 10 + 1) 1_000_000_007
        VGM.write p (2 * 10 + 2) 1_000_000_007
        pure p

  for_
    [ (inf, inf),
      (1, inf),
      (inf, 1),
      (7, inf),
      (inf `div` 337, 337),
      (2, (inf - 1) `div` 2)
    ]
    $ \(!a_, !b_) -> do
      for_ [0 .. 1] $ \ph -> do
        let (!a, !b)
              | ph == 0 = (a_, b_)
              | otherwise = (b_, a_)
        VU.forM_ ps $ \ans -> do
          let (!res1, !res2) = AM.crt (VU.fromList [ans `mod` a, ans `mod` b]) (VU.fromList [a, b])
          let lcm = a `div` gcd a b * b
          lcm @=? res2
          ans `mod` lcm @=? res1

  factorInf <- VU.unsafeThaw $ VU.fromList [49 :: Int, 73, 127, 337, 92737, 649657]
  fix $ \loop -> do
    factors <- VU.unsafeFreeze factorInf
    VU.forM_ ps $ \ans -> do
      let r = VU.map (\f -> ans `mod` f) factors
      let (!res1, !res2) = AM.crt r factors
      ans `mod` inf @=? res1
      inf @=? res2
    b <- VUM.nextPermutation factorInf
    when b loop

  factorInf1 <- VU.unsafeThaw $ VU.fromList [2 :: Int, 3, 715827883, 2147483647]
  fix $ \loop -> do
    factors <- VU.unsafeFreeze factorInf1
    VU.forM_ ps $ \ans -> do
      let r = VU.map (\f -> ans `mod` f) factors
      let (!res1, !res2) = AM.crt r factors
      ans `mod` (inf - 1) @=? res1
      (inf - 1) @=? res2
    b <- VUM.nextPermutation factorInf1
    when b loop

  pure ()

tests :: [TestTree]
tests =
  [ unit_powMod,
    unit_invBoundHand,
    unit_invMod,
    unit_invModZero,
    unit_floorSum,
    unit_crtHand,
    unit_crt2,
    unit_crt3,
    unit_crtOverflow,
    unit_crtBound
  ]
