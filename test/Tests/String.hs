module Tests.String (tests) where

import AtCoder.Internal.Assert qualified as ACIA
import AtCoder.Internal.String qualified as ACIS
import AtCoder.String qualified as AS
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Control.Monad.ST (runST)
import Data.ByteString.Char8 qualified as BS
import Data.Char (ord)
import Data.Foldable
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro qualified as VAI
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec

saNaive :: VU.Vector Int -> VU.Vector Int
saNaive s = VU.create $ do
  let n = VU.length s
  sa <- VUM.generate n id
  let f l r = compare (VU.drop l s) (VU.drop r s)
  VAI.sortBy f sa
  pure sa

lcpNaive :: VU.Vector Int -> VU.Vector Int -> VU.Vector Int
lcpNaive bs sa = VU.create $ do
  let n = VU.length bs
  let !_ = ACIA.runtimeAssert (n > 0) ""
  lcp <- VUM.replicate (n - 1) (0 :: Int)
  for_ [0 .. n - 2] $ \i -> do
    let l = sa VG.! i
    let r = sa VG.! (i + 1)
    fix $ \loop -> do
      lcpI <- VGM.read lcp i
      let sl = bs VU.! (l + lcpI)
      let sr = bs VU.! (r + lcpI)
      when (l + lcpI < n && r + lcpI < n && sl == sr) $ do
        VGM.modify lcp (+ 1) i
        loop
  pure lcp

zNaive :: VU.Vector Int -> VU.Vector Int
zNaive s = VU.create $ do
  let n = VU.length s
  z <- VUM.replicate n (0 :: Int)
  for_ [0 .. n - 1] $ \i -> do
    fix $ \loop -> do
      zi <- VGM.read z i
      let sl = s VU.! zi
      let sr = s VU.! (i + zi)
      when (i + zi < n && sl == sr) $ do
        VGM.modify z (+ 1) i
        loop
  pure z

unit_empty :: TestTree
unit_empty = testCase "empty" $ do
  VU.empty @=? AS.suffixArrayBS BS.empty
  VU.empty @=? AS.suffixArrayOrd (VU.empty @Int)

  VU.empty @=? AS.zAlgorithmBS BS.empty
  VU.empty @=? AS.zAlgorithm (VU.empty @Int)

testWithNaive :: (VU.Vector Int -> VU.Vector Int -> Int -> Assertion) -> Assertion
testWithNaive runTest = do
  for_ [1 .. 5] $ \n -> do
    let m = 4 ^ n :: Int
    for_ [0 .. m - 1] $ \f -> do
      let (!str, !maxC') = runST $ do
            s <- VUM.replicate n (0 :: Int)
            (!_, !maxC) <-
              VU.foldM'
                ( \(!g, !maxC_) i -> do
                    VGM.write s i $ g `mod` 4
                    pure (g `div` 4, max maxC_ (g `mod` 4))
                )
                (f, 0)
                (VU.generate n id)
            (,maxC) <$> VU.unsafeFreeze s
      runTest str (saNaive str) maxC'

  for_ [1 .. 10] $ \n -> do
    let m = 2 ^ n :: Int
    for_ [0 .. m - 1] $ \f -> do
      let (!str, !maxC') = runST $ do
            s <- VUM.replicate n (0 :: Int)
            (!_, !maxC) <-
              VU.foldM'
                ( \(!g, !maxC_) i -> do
                    VGM.write s i $ g `mod` 2
                    pure (g `div` 2, max maxC_ (g `mod` 2))
                )
                (f, 0)
                (VU.generate n id)
            (,maxC) <$> VU.unsafeFreeze s
      runTest str (saNaive str) maxC'

unit_saLcpNaive :: TestTree
unit_saLcpNaive = testCase "saLcpNaive" $ do
  testWithNaive $ \s sa upper -> do
    sa @=? AS.suffixArrayOrd s
    sa @=? AS.suffixArray s upper
    lcpNaive s sa @=? AS.lcpArray s sa

unit_saNaiveNaive :: TestTree
unit_saNaiveNaive = testCase "saNaiveNaive" $ do
  testWithNaive $ \s sa _upper -> do
    sa @=? ACIS.saNaive s

unit_saDoublingNaive :: TestTree
unit_saDoublingNaive = testCase "saDoublingNaive" $ do
  testWithNaive $ \s sa _upper -> do
    sa @=? ACIS.saDoubling s

unit_saIsNaive :: TestTree
unit_saIsNaive = testCase "saIsNaive" $ do
  testWithNaive $ \s sa upper -> do
    sa @=? ACIS.saIsManual (-1) (-1) s upper

unit_allATest :: TestTree
unit_allATest = testCase "allATest" $ do
  for_ [1 .. 100] $ \n -> do
    let s = VU.replicate n (0 :: Int)
    saNaive s @=? AS.suffixArrayOrd s
    saNaive s @=? AS.suffixArray s 10
    saNaive s @=? AS.suffixArray s 12

unit_allBTest :: TestTree
unit_allBTest = testCase "allBTest" $ do
  for_ [1 .. 100] $ \n -> do
    let s = VU.generate n (`mod` 2)
    saNaive s @=? AS.suffixArrayOrd s
    saNaive s @=? AS.suffixArray s 3
  for_ [1 .. 100] $ \n -> do
    let s = VU.generate n ((1 -) . (`mod` 2))
    saNaive s @=? AS.suffixArrayOrd s
    saNaive s @=? AS.suffixArray s 3

-- TODO: all A test
-- TODO: all B test

unit_sa :: TestTree
unit_sa = testCase "sa" $ do
  let s = BS.pack "missisippi"
  let ans =
        V.map BS.pack $
          V.fromList
            [ "i",
              "ippi",
              "isippi",
              "issisippi",
              "missisippi",
              "pi",
              "ppi",
              "sippi",
              "sisippi",
              "ssisippi"
            ]

  testSA "public" s ans $ AS.suffixArrayBS s
  testSA "internal naive" s ans $ ACIS.saNaive (bsToSA s)
  testSA "internal doubling" s ans $ ACIS.saDoubling (bsToSA s)
  testSA "internal SA-IS" s ans $ ACIS.saIsImpl 10 40 (bsToSA s) 255
  where
    testSA msg s expected result = do
      VU.length result @?= V.length expected
      V.iforM_ expected $ \i ansI -> do
        let res = BS.drop (result VG.! i) s
        assertBool (msg ++ ": " ++ show result) (res == ansI)
    bsToSA :: BS.ByteString -> VU.Vector Int
    bsToSA s =
      let n = BS.length s
       in -- FIXME: correct? (upper 255?)
          VU.map ord $ VU.fromListN n (BS.unpack s)

unit_saSingle :: TestTree
unit_saSingle = testCase "saSingle" $ do
  VU.singleton 0 @?= AS.suffixArrayOrd (VU.singleton (0 :: Int))
  VU.singleton 0 @?= AS.suffixArrayOrd (VU.singleton (-1 :: Int))
  VU.singleton 0 @?= AS.suffixArrayOrd (VU.singleton (-1 :: Int))
  VU.singleton 0 @?= AS.suffixArrayOrd (VU.singleton (minBound :: Int))
  VU.singleton 0 @?= AS.suffixArrayOrd (VU.singleton (maxBound :: Int))

unit_lcp :: TestTree
unit_lcp = testCase "LCP" $ do
  let s = BS.pack "aab"
  let sa = AS.suffixArrayBS s
  VU.fromList [0, 1, 2] @=? sa
  let lcp = AS.lcpArrayBS s sa
  VU.fromList [1, 0] @=? lcp

  lcp @=? AS.lcpArray (VU.fromList [0 :: Int, 0, 1]) sa
  lcp @=? AS.lcpArray (VU.fromList [-100 :: Int, -100, 100]) sa
  lcp @=? AS.lcpArray (VU.fromList [minBound :: Int, minBound, maxBound]) sa

unit_zAlgo :: TestTree
unit_zAlgo = testCase "zAlgo" $ do
  let s = BS.pack "abab"
  let z = AS.zAlgorithmBS s
  VU.fromList [4, 0, 2, 0] @=? z
  VU.fromList [4, 0, 2, 0] @=? AS.zAlgorithm (VU.fromList [1 :: Int, 10, 1, 10])
  zNaive (VU.replicate 7 0) @=? AS.zAlgorithm (VU.replicate 7 (0 :: Int))

unit_zNaive :: TestTree
unit_zNaive = testCase "zNaive" $ do
  for_ [1 .. 6] $ \n -> do
    let m = 4 ^ n :: Int
    for_ [0 .. m - 1] $ \f -> do
      let s' = VU.create $ do
            s <- VUM.replicate n (0 :: Int)
            VU.foldM'_
              ( \g i -> do
                  VGM.write s i $ g `mod` 4
                  pure $ g `div` 4
              )
              f
              (VU.generate n id)
            pure s
      zNaive s' @=? AS.zAlgorithm s'

  for_ [1 .. 10] $ \n -> do
    let m = 2 ^ n
    for_ [0 .. m - 1] $ \f -> do
      let s' = VU.create $ do
            s <- VUM.replicate n (0 :: Int)
            VU.foldM'_
              ( \g i -> do
                  VGM.write s i $ g `mod` 2
                  pure $ g `div` 2
              )
              f
              (VU.generate n id)
            pure s
      zNaive s' @=? AS.zAlgorithm s'

spec_invalid :: IO TestTree
spec_invalid = testSpec "invalid" $ do
  -- safe:
  let !_ = AS.suffixArray (VU.fromList [0, 10]) 10
  it "throws error" $ (`shouldThrow` anyException) $ do
    let !_ = AS.suffixArray VU.empty (-1)
    pure ()
  it "throws error" $ (`shouldThrow` anyException) $ do
    let !_ = AS.suffixArray (VU.singleton (-1)) 10
    pure ()
  it "throws error" $ (`shouldThrow` anyException) $ do
    let !_ = AS.suffixArray (VU.singleton 11) 10
    pure ()
  it "throws error" $ (`shouldThrow` anyException) $ do
    let !_ = AS.suffixArray (VU.fromList [0, 11]) 10
    pure ()
  it "throws error" $ (`shouldThrow` anyException) $ do
    let !_ = AS.suffixArray (VU.fromList [0, -1]) 10
    pure ()

  -- safe:
  let !_ = AS.suffixArrayOrd (VU.fromList [-999 :: Int, 999])
  pure ()

tests :: [TestTree]
tests =
  [ unit_empty,
    unit_saLcpNaive,
    unit_saNaiveNaive,
    unit_saDoublingNaive,
    unit_saIsNaive,
    unit_allATest,
    unit_allBTest,
    unit_sa,
    unit_saSingle,
    unit_lcp,
    unit_zAlgo,
    unit_zNaive,
    unsafePerformIO spec_invalid
  ]
