{-# LANGUAGE RecordWildCards #-}

-- | Minimum parse/print with @bytestring@.
module Util
  ( -- * Parser
    Parser,

    -- * Parse functions
    intP,
    mintP,
    intS1P,
    int2P,
    int3P,
    int4P,

    -- * Get stdin
    int,
    ints2,
    ints3,
    ints4,
    ints,

    -- * Show with
    withLine,
    wsBSB,
    endlBSB,
    ynBSB,
    unlinesBSB,
    unlinesWithBSB,
    unwordsBSB,
    unwordsWithBSB,

    -- * Show
    show2,
    showMat,

    -- Print
    putBSB,
    printBSB,
  )
where

import AtCoder.Extra.Semigroup.Matrix qualified as Mat
import AtCoder.ModInt qualified as M
import Control.Monad.Trans.State.Strict (StateT (..), evalStateT)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Char8 qualified as BS
import Data.Maybe (fromJust, fromMaybe)
import Data.Vector qualified as V
import Data.Vector.Generic qualified as VG
import Data.Vector.Unboxed qualified as VU
import GHC.Stack (HasCallStack)
import GHC.TypeNats (KnownNat)
import System.IO (stdout)

-- TODO: use MonadState

-- | Failable parser.
type Parser = StateT BS.ByteString Maybe

-- | Parses an `Int`.
{-# INLINE intP #-}
intP :: Parser Int
intP = StateT $ BS.readInt . BS.dropSpace

-- | Parses an `ModInt`.
{-# INLINE mintP #-}
mintP :: (KnownNat p) => Parser (M.ModInt p)
mintP = StateT $ ((\(!a, !b) -> (M.new a, b)) <$>) . BS.readInt . BS.dropSpace

-- | Parses an `Int` and subtracts @1@.
{-# INLINE intS1P #-}
intS1P :: Parser Int
intS1P = StateT $ ((\(!x, !bs) -> (x - 1, bs)) <$>) . BS.readInt . BS.dropSpace

{-# INLINE int2P #-}
int2P :: (HasCallStack) => Parser (Int, Int)
int2P = do
  x1 <- intP
  x2 <- intP
  pure (x1, x2)

{-# INLINE int3P #-}
int3P :: (HasCallStack) => Parser (Int, Int, Int)
int3P = do
  x1 <- intP
  x2 <- intP
  x3 <- intP
  pure (x1, x2, x3)

{-# INLINE int4P #-}
int4P :: (HasCallStack) => Parser (Int, Int, Int, Int)
int4P = do
  x1 <- intP
  x2 <- intP
  x3 <- intP
  x4 <- intP
  pure (x1, x2, x3, x4)

-- * Line getter

-- | Gets @a@.
{-# INLINE int #-}
int :: (HasCallStack) => IO Int
int = fromMaybe (error "int") . evalStateT intP <$> BS.getLine

-- | Gets @a b@.
{-# INLINE ints2 #-}
ints2 :: (HasCallStack) => IO (Int, Int)
ints2 = fromMaybe (error "ints2") . evalStateT int2P <$> BS.getLine

-- | Gets @a b c@.
{-# INLINE ints3 #-}
ints3 :: (HasCallStack) => IO (Int, Int, Int)
ints3 = fromMaybe (error "ints3") . evalStateT int3P <$> BS.getLine

-- | Gets @a b c d@.
{-# INLINE ints4 #-}
ints4 :: (HasCallStack) => IO (Int, Int, Int, Int)
ints4 = fromMaybe (error "ints4") . evalStateT int4P <$> BS.getLine

-- | Gets @a b c ..@.
{-# INLINE ints #-}
ints :: IO (VU.Vector Int)
ints = VU.unfoldr (BS.readInt . BS.dropSpace) <$> BS.getLine

-- | Reads one line from the state and runs a pure parser for it.
{-# INLINE withLine #-}
withLine :: (HasCallStack) => Parser a -> IO a
withLine f = fromJust . evalStateT f <$> BS.getLine

-- * Handy bytestirng @Builder@

{-# INLINE wsBSB #-}
wsBSB :: BSB.Builder
wsBSB = BSB.char7 ' '

{-# INLINE endlBSB #-}
endlBSB :: BSB.Builder
endlBSB = BSB.char7 '\n'

{-# INLINE ynBSB #-}
ynBSB :: Bool -> BSB.Builder
ynBSB True = BSB.string7 "Yes"
ynBSB False = BSB.string7 "No"

{-# INLINE intersperseWithBSB #-}
intersperseWithBSB :: (VG.Vector v a) => (a -> BSB.Builder) -> BSB.Builder -> v a -> BSB.Builder
intersperseWithBSB showF del vec
  | VG.null vec = mempty
  | otherwise = showF (VG.head vec) <> VG.foldMap ((del <>) . showF) (VG.tail vec)

{-# INLINE unwordsBSB #-}
unwordsBSB :: (VG.Vector v Int) => v Int -> BSB.Builder
unwordsBSB = unwordsWithBSB BSB.intDec

{-# INLINE unwordsWithBSB #-}
unwordsWithBSB :: (VG.Vector v a) => (a -> BSB.Builder) -> v a -> BSB.Builder
unwordsWithBSB f = intersperseWithBSB f wsBSB

{-# INLINE unlinesBSB #-}
unlinesBSB :: (VG.Vector v Int) => v Int -> BSB.Builder
unlinesBSB = intersperseWithBSB BSB.intDec endlBSB

{-# INLINE unlinesWithBSB #-}
unlinesWithBSB :: (VG.Vector v a) => (a -> BSB.Builder) -> v a -> BSB.Builder
unlinesWithBSB showF = intersperseWithBSB showF endlBSB

{-# INLINE show2 #-}
show2 :: (Int, Int) -> BSB.Builder
show2 (!a, !b) = BSB.intDec a <> wsBSB <> BSB.intDec b

{-# INLINE showMat #-}
showMat :: (Show a, VU.Unbox a) => Mat.Matrix a -> BSB.Builder
showMat Mat.Matrix {..} = unlinesWithBSB id $ V.map showF rows
  where
    rows = V.unfoldrExactN hM (VU.splitAt wM) vecM
    showF = unwordsWithBSB (BSB.string8 . show)

{-# INLINE putBSB #-}
putBSB :: BSB.Builder -> IO ()
putBSB = BSB.hPutBuilder stdout

{-# INLINE printBSB #-}
printBSB :: BSB.Builder -> IO ()
printBSB = putBSB . (<> endlBSB)
