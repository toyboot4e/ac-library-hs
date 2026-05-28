{-# LANGUAGE ViewPatterns #-}

import AtCoder.Extra.SegTree2d.Dense qualified as Seg
import AtCoder.Internal.Queue qualified as Q
import Control.Monad (unless)
import Data.Semigroup (Sum (..))
import Data.Vector.Unboxed qualified as VU
import Util

-- verification-helper: PROBLEM https://judge.u-aizu.ac.jp/onlinejudge/description.jsp?id=2842
main :: IO ()
main = do
  (!w, !h, !dt, !q) <- ints4
  qs <- VU.replicateM q $
    withLine $ do
      t <- intP
      c <- intP
      case c of
        0 -> (t,c,,,-1,-1) <$> intP <*> intP
        1 -> (t,c,,,-1,-1) <$> intP <*> intP
        2 -> (t,c,,,,) <$> intP <*> intP <*> intP <*> intP
        _ -> error "unreachable"

  segA <- Seg.new w h
  segB <- Seg.new w h
  que <- Q.new q

  res <- (`VU.mapMaybeM` qs) $ \query -> do
    let (!t0, !_, !_, !_, !_, !_) = query
    let popLoop = do
          z <- Q.peekFront que
          case z of
            Just (!x, !y, !t)
              | t <= t0 -> do
                  Q.popFront_ que
                  Seg.write segA x y $ Sum 1
                  Seg.write segB x y $ Sum 0
                  popLoop
            _ -> pure ()
    popLoop

    case query of
      (!t, 0, pred -> !x, pred -> !y, !_, !_) -> do
        -- write 1
        Seg.write segB x y $ Sum 1
        Q.pushBack que (x, y, t + dt)
        pure Nothing
      (!_, 1, pred -> !x, pred -> !y, !_, !_) -> do
        -- write 0
        Seg.write segA x y $ Sum 0
        pure Nothing
      (!_, 2, pred -> !x1, pred -> !y1, !x2, !y2) -> do
        -- prod
        Sum a <- Seg.prod segA x1 x2 y1 y2
        Sum b <- Seg.prod segB x1 x2 y1 y2
        pure $ Just (a, b)
      _ -> error "unreachable"

  unless (VU.null res) $ do
    printBSB $ unlinesWithBSB show2 res
