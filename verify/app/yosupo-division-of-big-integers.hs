import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Builder qualified as BSB
import Data.Maybe (fromJust)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import System.IO (stdout)

{-# INLINE putBSB #-}
putBSB :: BSB.Builder -> IO ()
putBSB = BSB.hPutBuilder stdout

{-# INLINE printBSB #-}
printBSB :: BSB.Builder -> IO ()
printBSB = putBSB . (<> endlBSB)

{-# INLINE wsBSB #-}
wsBSB :: BSB.Builder
wsBSB = BSB.char7 ' '

{-# INLINE endlBSB #-}
endlBSB :: BSB.Builder
endlBSB = BSB.char7 '\n'

{-# INLINE intersperseWithBSB #-}
intersperseWithBSB :: (VG.Vector v a) => (a -> BSB.Builder) -> BSB.Builder -> v a -> BSB.Builder
intersperseWithBSB showF del vec
  | VG.null vec = mempty
  | otherwise = showF (VG.head vec) <> VG.foldMap ((del <>) . showF) (VG.tail vec)

{-# INLINE unlinesWithBSB #-}
unlinesWithBSB :: (VG.Vector v a) => (a -> BSB.Builder) -> v a -> BSB.Builder
unlinesWithBSB showF = intersperseWithBSB showF endlBSB

{-# INLINE show2 #-}
show2 :: (Integer, Integer) -> BSB.Builder
show2 (!a, !b) = BSB.integerDec a <> wsBSB <> BSB.integerDec b

-- verification-helper: PROBLEM https://judge.yosupo.jp/problem/division_of_big_integers
main :: IO ()
main = do
  (!t, !_) <- fromJust . BS.readInt <$> BS.getLine
  abs <- V.replicateM t $ do
    s <- BS.getLine
    let (!a, !s') = fromJust $ BS.readInteger s
    let (!b, !_) = fromJust . BS.readInteger $ BS.dropSpace s'
    let (!q, !r) = a `divMod` b
    pure (q, r)
  printBSB $ unlinesWithBSB show2 abs
