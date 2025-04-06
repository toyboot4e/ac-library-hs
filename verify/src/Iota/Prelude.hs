-- | From `https://github.com/cojna/iota` (CC0 license)

{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Iota.Prelude where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Data.Bits
import Data.Bool
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Prim as BP
import qualified Data.ByteString.Builder.Prim.Internal as BP
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Short as B (ShortByteString, toShort)
import qualified Data.ByteString.Short as B.Short
import qualified Data.Foldable as F
import Data.Functor.Identity
import Data.Primitive
import qualified Data.Vector as V
import qualified Data.Vector.Fusion.Bundle as Bundle
import qualified Data.Vector.Fusion.Bundle.Monadic as MBundle
import qualified Data.Vector.Fusion.Bundle.Size as Bundle
import qualified Data.Vector.Fusion.Stream.Monadic as MS
import Data.Vector.Fusion.Util
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Base as U (Vector (..))
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import GHC.Exts
import System.IO

import Iota.PrimParser

-- * Stream utils
rep :: (Monad m) => Int -> (Int -> m ()) -> m ()
rep n = flip MS.mapM_ (0 ..< n)
{-# INLINE rep #-}

rep1 :: (Monad m) => Int -> (Int -> m ()) -> m ()
rep1 n = flip MS.mapM_ (1 ..< n + 1)
{-# INLINE rep1 #-}

rev :: (Monad m) => Int -> (Int -> m ()) -> m ()
rev n = flip MS.mapM_ (n >.. 0)
{-# INLINE rev #-}

rev1 :: (Monad m) => Int -> (Int -> m ()) -> m ()
rev1 n = flip MS.mapM_ (n + 1 >.. 1)
{-# INLINE rev1 #-}

infix 4 ..<

{- |
>>> runIdentity $ MS.toList (0..<5)
[0,1,2,3,4]
-}
(..<) :: (Monad m) => Int -> Int -> MS.Stream m Int
(..<) !l !r = MS.Stream step l
  where
    step x
      | x < r = return $ MS.Yield x (x + 1)
      | otherwise = return MS.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] (..<) #-}

infix 4 >..

{- |
>>> runIdentity $ MS.toList (5>..0)
[4,3,2,1,0]
-}
(>..) :: (Monad m) => Int -> Int -> MS.Stream m Int
(>..) !r !l = MS.Stream step (r - 1)
  where
    step x
      | x >= l = return $ MS.Yield x (x - 1)
      | otherwise = return MS.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] (>..) #-}

{- |
>>> runIdentity $ MS.toList (stride 0 10 3)
[0,3,6,9]
-}
stride :: (Monad m) => Int -> Int -> Int -> MS.Stream m Int
stride !l !r !d = MS.Stream step l
  where
    step x
      | x < r = return $ MS.Yield x (x + d)
      | otherwise = return MS.Done
    {-# INLINE [0] step #-}
{-# INLINE [1] stride #-}

liftMS :: (Monad m) => MS.Stream Id a -> MS.Stream m a
liftMS = MS.trans (return . unId)
{-# INLINE liftMS #-}

-- * Vector utils

asUVector :: U.Vector a -> U.Vector a
asUVector = id
{-# INLINE asUVector #-}

asBVector :: V.Vector a -> V.Vector a
asBVector = id
{-# INLINE asBVector #-}

{- |
>>> lowerBound (U.fromList "122333") '2'
1
>>> lowerBound (U.fromList "122333") '0'
0
>>> lowerBound (U.fromList "122333") '9'
6
-}
lowerBound :: (Ord a, G.Vector v a) => v a -> a -> Int
lowerBound !vec !key = binarySearch 0 (G.length vec) ((key <=) . G.unsafeIndex vec)
{-# INLINE lowerBound #-}

{- |
>>> upperBound (U.fromList "122333") '2'
3
>>> upperBound (U.fromList "122333") '0'
0
>>> upperBound (U.fromList "122333") '9'
6
-}
upperBound :: (Ord a, G.Vector v a) => v a -> a -> Int
upperBound !vec !key = binarySearch 0 (G.length vec) ((key <) . G.unsafeIndex vec)
{-# INLINE upperBound #-}

{- |
>>> radixSort $ U.fromList [3,1,4,1,5,9]
[1,1,3,4,5,9]
>>> radixSort $ U.fromList [-3,-1,-4,1,5,9]
[1,5,9,-4,-3,-1]
-}
radixSort :: U.Vector Int -> U.Vector Int
radixSort v0 = F.foldl' step v0 [0, 16, 32, 48]
  where
    step v k = U.create $ do
      pos <- UM.unsafeNew 0x10001
      UM.set pos 0
      U.forM_ v $ \x -> do
        UM.unsafeModify pos (+ 1) ((x !>>>. k) .&. 0xffff + 1)
      rep 0xffff $ \i -> do
        fi <- UM.unsafeRead pos i
        UM.unsafeModify pos (+ fi) (i + 1)
      res <- UM.unsafeNew $ U.length v
      U.forM_ v $ \x -> do
        let !masked = (x !>>>. k) .&. 0xffff
        i <- UM.unsafeRead pos masked
        UM.unsafeWrite pos masked $ i + 1
        UM.unsafeWrite res i x
      return res
{-# INLINE radixSort #-}

{- |
>>> runLengthEncode $ U.fromList "abbccc"
[('a',1),('b',2),('c',3)]
>>> runLengthEncode $ U.fromList ""
[]
-}
runLengthEncode ::
  (Eq a, G.Vector v a, G.Vector v (a, Int)) =>
  v a ->
  v (a, Int)
runLengthEncode =
  G.unstream
    . Bundle.inplace streamRLE Bundle.toMax
    . G.stream
{-# INLINE runLengthEncode #-}

streamRLE :: (Eq a, Monad m) => MS.Stream m a -> MS.Stream m (a, Int)
streamRLE (MS.Stream step s0) = MS.Stream step' (Nothing, s0)
  where
    step' (Nothing, s) = do
      r <- step s
      case r of
        MS.Yield x s' -> return $ MS.Skip (Just (x, 1), s')
        MS.Skip s' -> return $ MS.Skip (Nothing, s')
        MS.Done -> return MS.Done
    step' (Just (x, !i), s) = do
      r <- step s
      case r of
        MS.Yield y s'
          | x == y -> return $ MS.Skip (Just (x, i + 1), s')
          | otherwise -> return $ MS.Yield (x, i) (Just (y, 1), s')
        MS.Skip s' -> return $ MS.Skip (Just (x, i), s')
        MS.Done -> return $ MS.Yield (x, i) (Nothing, s)
    {-# INLINE [0] step' #-}
{-# INLINE [1] streamRLE #-}

{- |
>>> forAccum (0 :: Int) (U.fromList "abc") $ \acc c -> (acc + 1, (acc, c))
[(0,'a'),(1,'b'),(2,'c')]
-}
forAccum ::
  (G.Vector v a, G.Vector v b) =>
  s ->
  v a ->
  (s -> a -> (s, b)) ->
  v b
forAccum x v f = mapAccum f x v
{-# INLINE forAccum #-}

mapAccum ::
  (G.Vector v a, G.Vector v b) =>
  (s -> a -> (s, b)) ->
  s ->
  v a ->
  v b
mapAccum f x =
  G.unstream
    . Bundle.inplace
      (streamAccumM (\s a -> pure (f s a)) x)
      Bundle.toMax
    . G.stream
{-# INLINE mapAccum #-}

forAccumM ::
  (PrimMonad m, G.Vector v a, G.Vector v b) =>
  s ->
  v a ->
  (s -> a -> m (s, b)) ->
  m (v b)
forAccumM s v f = mapAccumM f s v
{-# INLINE forAccumM #-}

forAccumM_ ::
  (Monad m, G.Vector v b) =>
  a ->
  v b ->
  (a -> b -> m a) ->
  m ()
forAccumM_ x v f = void $ G.foldM'_ f x v
{-# INLINE forAccumM_ #-}

mapAccumM ::
  (PrimMonad m, G.Vector v a, G.Vector v b) =>
  (s -> a -> m (s, b)) ->
  s ->
  v a ->
  m (v b)
mapAccumM f x =
  (>>= G.unsafeFreeze)
    . GM.munstream
    . bundleAccumM f x
    . Bundle.lift
    . G.stream
{-# INLINE mapAccumM #-}

forAccumMaybe ::
  (G.Vector v a, G.Vector v b) =>
  s ->
  v a ->
  (s -> a -> (s, Maybe b)) ->
  v b
forAccumMaybe x v f = mapAccumMaybe f x v
{-# INLINE forAccumMaybe #-}

mapAccumMaybe ::
  (G.Vector v a, G.Vector v b) =>
  (s -> a -> (s, Maybe b)) ->
  s ->
  v a ->
  v b
mapAccumMaybe f x =
  G.unstream
    . Bundle.inplace
      (streamAccumMaybeM (\s a -> pure (f s a)) x)
      Bundle.toMax
    . G.stream
{-# INLINE mapAccumMaybe #-}

forAccumMaybeM ::
  (PrimMonad m, G.Vector v a, G.Vector v b) =>
  s ->
  v a ->
  (s -> a -> m (s, Maybe b)) ->
  m (v b)
forAccumMaybeM s v f = mapAccumMaybeM f s v
{-# INLINE forAccumMaybeM #-}

mapAccumMaybeM ::
  (PrimMonad m, G.Vector v a, G.Vector v b) =>
  (s -> a -> m (s, Maybe b)) ->
  s ->
  v a ->
  m (v b)
mapAccumMaybeM f x =
  (>>= G.unsafeFreeze)
    . GM.munstream
    . bundleAccumMaybeM f x
    . Bundle.lift
    . G.stream
{-# INLINE mapAccumMaybeM #-}

bundleAccumM ::
  (Monad m) =>
  (s -> a -> m (s, b)) ->
  s ->
  MBundle.Bundle m v a ->
  MBundle.Bundle m v b
bundleAccumM f x bundle =
  MBundle.fromStream
    (streamAccumM f x (MBundle.elements bundle))
    (MBundle.size bundle)
{-# INLINE [1] bundleAccumM #-}

bundleAccumMaybeM ::
  (Monad m) =>
  (s -> a -> m (s, Maybe b)) ->
  s ->
  MBundle.Bundle m v a ->
  MBundle.Bundle m v b
bundleAccumMaybeM f x bundle =
  MBundle.fromStream
    (streamAccumMaybeM f x (MBundle.elements bundle))
    (MBundle.size bundle)
{-# INLINE [1] bundleAccumMaybeM #-}

streamAccumM :: (Monad m) => (s -> a -> m (s, b)) -> s -> MS.Stream m a -> MS.Stream m b
streamAccumM f s0 (MS.Stream step x0) = MS.Stream step' (s0, x0)
  where
    step' (!s, x) = do
      r <- step x
      case r of
        MS.Yield a x' -> do
          (s', b) <- f s a
          return $ MS.Yield b (s', x')
        MS.Skip x' -> return $ MS.Skip (s, x')
        MS.Done -> return MS.Done
    {-# INLINE [0] step' #-}
{-# INLINE [1] streamAccumM #-}

streamAccumMaybeM :: (Monad m) => (s -> a -> m (s, Maybe b)) -> s -> MS.Stream m a -> MS.Stream m b
streamAccumMaybeM f s0 (MS.Stream step x0) = MS.Stream step' (s0, x0)
  where
    step' (!s, x) = do
      r <- step x
      case r of
        MS.Yield a x' -> do
          (s', mb) <- f s a
          return $ case mb of
            Just b -> MS.Yield b (s', x')
            Nothing -> MS.Skip (s', x')
        MS.Skip x' -> return $ MS.Skip (s, x')
        MS.Done -> return MS.Done
    {-# INLINE [0] step' #-}
{-# INLINE [1] streamAccumMaybeM #-}

stream :: (G.Vector v a) => v a -> MS.Stream Id a
stream = MBundle.elements . G.stream
{-# INLINE stream #-}

streamM :: (G.Vector v a, Monad m) => v a -> MS.Stream m a
streamM = MS.trans (return . unId) . MBundle.elements . G.stream
{-# INLINE streamM #-}

{- |
>>> asUVector . unstream 10 . stream $ U.fromList "abc"
"abc"
-}
unstream :: (G.Vector v a) => Int -> MS.Stream Id a -> v a
unstream ub =
  G.unstream
    . flip MBundle.fromStream (Bundle.Max ub)
{-# INLINE unstream #-}

unstreamM ::
  (PrimMonad m, G.Vector v a) =>
  Int ->
  MS.Stream m a ->
  m (v a)
unstreamM ub s =
  GM.munstream
    (MBundle.fromStream s (Bundle.Max ub))
    >>= G.unsafeFreeze
{-# INLINE [1] unstreamM #-}

-- * Bits utils
infixl 8 `shiftRL`, `unsafeShiftRL`, !>>>.

shiftRL :: Int -> Int -> Int
shiftRL = unsafeShiftRL
{-# INLINE shiftRL #-}

{- |
>>> unsafeShiftR (-1) 1
-1
>>> unsafeShiftRL (-1) 1
9223372036854775807
-}
unsafeShiftRL :: Int -> Int -> Int
unsafeShiftRL (I# x#) (I# i#) = I# (uncheckedIShiftRL# x# i#)
{-# INLINE unsafeShiftRL #-}

(!>>>.) :: Int -> Int -> Int
(!>>>.) = unsafeShiftRL
{-# INLINE (!>>>.) #-}

{- |
BSR (Bit Scan Reverse)

>>> floorLog2 0
-1
>>> floorLog2 1
0
>>> floorLog2 2
1
>>> floorLog2 1023
9
>>> floorLog2 1024
10
>>> floorLog2 1025
10
>>> floorLog2 maxBound
62
-}
floorLog2 :: Int -> Int
floorLog2 x = 63 - countLeadingZeros x
{-# INLINE floorLog2 #-}

{- |
>>> ceilingLog2 0
0
>>> ceilingLog2 1
0
>>> ceilingLog2 2
1
>>> ceilingLog2 1023
10
>>> ceilingLog2 1024
10
>>> ceilingLog2 1025
11
>>> ceilingLog2 maxBound
63
-}
ceilingLog2 :: Int -> Int
ceilingLog2 x
  | x > 1 = 64 - countLeadingZeros (x - 1)
  | otherwise = 0
{-# INLINE ceilingLog2 #-}

{- |
>>> floorPowerOf2 0
-9223372036854775808
>>> floorPowerOf2 1
1
>>> floorPowerOf2 2
2
>>> floorPowerOf2 1023
512
>>> floorPowerOf2 1024
1024
>>> floorPowerOf2 1025
1024
>>> floorPowerOf2 maxBound
4611686018427387904
-}
floorPowerOf2 :: Int -> Int
floorPowerOf2 x = 1 !<<. floorLog2 x
{-# INLINE floorPowerOf2 #-}

{- |
>>> ceilingPowerOf2 0
1
>>> ceilingPowerOf2 1
1
>>> ceilingPowerOf2 2
2
>>> ceilingPowerOf2 1023
1024
>>> ceilingPowerOf2 1024
1024
>>> ceilingPowerOf2 1025
2048
>>> ceilingPowerOf2 maxBound
-9223372036854775808
-}
ceilingPowerOf2 :: Int -> Int
ceilingPowerOf2 x
  | x > 1 = unsafeShiftRL (-1) (countLeadingZeros (x - 1)) + 1
  | otherwise = 1
{-# INLINE ceilingPowerOf2 #-}

-- * Parser utils
uvectorN :: (U.Unbox a) => Int -> PrimParser a -> PrimParser (U.Vector a)
uvectorN = gvectorN
{-# INLINE uvectorN #-}

bvectorN :: Int -> PrimParser a -> PrimParser (V.Vector a)
bvectorN = gvectorN
{-# INLINE bvectorN #-}

gvectorN :: (G.Vector v a) => Int -> PrimParser a -> PrimParser (v a)
gvectorN n f = do
  (e, o) <- viewPrimParser
  pure $ G.unfoldrExactN n (runPrimParser f e) o
{-# INLINE gvectorN #-}

streamN :: Int -> PrimParser a -> PrimParser (MS.Stream Id a)
streamN n f = do
  (e, o) <- viewPrimParser
  pure $ MS.unfoldrExactN n (runPrimParser f e) o
{-# INLINE streamN #-}

gvectorLn :: (G.Vector v a) => PrimParser a -> PrimParser (v a)
gvectorLn f = PrimParser $ \e p ->
  case memchrP# e p 0xa of
    pos ->
      (#
        plusAddr# pos 1#
        , G.unfoldrExactN
            (I# (minusAddr# pos p))
            (runPrimParser f (Ptr pos))
            (Ptr p)
      #)
{-# INLINE gvectorLn #-}

uvectorLn :: (U.Unbox a) => PrimParser a -> PrimParser (U.Vector a)
uvectorLn = gvectorLn
{-# INLINE uvectorLn #-}

-- * ByteString utils
bsToBytes :: B.ByteString -> U.Vector Word8
bsToBytes bs = case B.toShort bs of
  B.Short.SBS ba# -> baToBytes (ByteArray ba#)

bsToChars :: B.ByteString -> U.Vector Char
bsToChars = U.map B.w2c . bsToBytes

baToBytes :: ByteArray -> U.Vector Word8
baToBytes ba = U.V_Word8 (P.Vector 0 (sizeofByteArray ba) ba)

baToChars :: ByteArray -> U.Vector Char
baToChars = U.map B.w2c . baToBytes

baToSBS :: ByteArray -> B.ShortByteString
baToSBS (ByteArray ba#) = B.Short.SBS ba#

-- * Builder utils
unlinesB :: (G.Vector v a) => (a -> B.Builder) -> v a -> B.Builder
unlinesB f = G.foldMap ((<> lfB) . f)

unwordsB :: (G.Vector v a) => (a -> B.Builder) -> v a -> B.Builder
unwordsB f vec
  | G.null vec = mempty
  | otherwise =
      f (G.head vec)
        <> G.foldMap ((spB <>) . f) (G.tail vec)

concatB :: (G.Vector v a) => (a -> B.Builder) -> v a -> B.Builder
concatB = G.foldMap

{- |
>>> matrixB B.intDec (2, 3, U.fromListN 6 [1, 2, 3, 4, 5, 6])
"1 2 3\n4 5 6\n"
-}
matrixB :: (G.Vector v a) => (a -> B.Builder) -> (Int, Int, v a) -> B.Builder
matrixB f (!h, !w, !mat) =
  U.foldMap
    ( \i ->
        unwordsB f (G.slice (i * w) w mat) <> lfB
    )
    $ U.generate h id

{- |
>>> gridB B.char7 (2, 3, U.fromListN 6 ".#.#.#")
".#.\n#.#\n"
>>> gridB B.intDec (2, 3, U.fromListN 6 [1, 2, 3, 4, 5, 6])
"123\n456\n"
-}
gridB :: (G.Vector v a) => (a -> B.Builder) -> (Int, Int, v a) -> B.Builder
gridB f (!h, !w, !mat) =
  U.foldMap
    ( \i ->
        G.foldMap f (G.slice (i * w) w mat) <> lfB
    )
    $ U.generate h id

sizedB :: (G.Vector v a) => (v a -> B.Builder) -> v a -> B.Builder
sizedB f vec = B.intDec (G.length vec) <> lfB <> f vec

{- |
>>> yesnoB True
"Yes"
>>> yesnoB False
"No"
-}
yesnoB :: Bool -> B.Builder
yesnoB = BP.primBounded $ BP.boundedPrim 4 $ \flg ptr -> do
  if flg
    then do
      poke (castPtr ptr) (0x00736559 :: Word32)
      return $! plusPtr ptr 3
    else do
      poke (castPtr ptr) (0x6f4e :: Word16)
      return $! plusPtr ptr 2
{-# INLINE yesnoB #-}

{- |
>>> pairB B.intDec B.intDec $ (0, 1)
"0 1"
>>> pairB B.intDec (pairB B.intDec B.intDec) $ (0, (1, 2))
"0 1 2"
-}
pairB :: (a -> B.Builder) -> (b -> B.Builder) -> (a, b) -> B.Builder
pairB f g (x, y) = f x <> spB <> g y

showB :: (Show a) => a -> B.Builder
showB = B.string7 . show

showLnB :: (Show a) => a -> B.Builder
showLnB = B.string7 . flip shows "\n"

{- |
>>> lfB
"\n"
-}
lfB :: B.Builder
lfB = B.word8 0x0a

{- |
>>> spB
" "
-}
spB :: B.Builder
spB = B.word8 0x20

putBuilder :: (MonadIO m) => B.Builder -> m ()
putBuilder = liftIO . B.hPutBuilder stdout

putBuilderLn :: (MonadIO m) => B.Builder -> m ()
putBuilderLn b = putBuilder b *> putBuilder lfB

-- * Misc
newtype YesNo = YesNo Bool

instance Show YesNo where
  show (YesNo True) = "Yes"
  show (YesNo False) = "No"

inGrid :: Int -> Int -> Int -> Int -> Bool
inGrid h w x y = 0 <= x && x < h && 0 <= y && y < w
{-# INLINE inGrid #-}

forNeighbor4_ :: (Applicative f) => Int -> Int -> Int -> Int -> (Int -> Int -> f ()) -> f ()
forNeighbor4_ h w x y f =
  when (x /= 0) (f (x - 1) y)
    *> when (y /= 0) (f x (y - 1))
    *> when (y /= w - 1) (f x (y + 1))
    *> when (x /= h - 1) (f (x + 1) y)
{-# INLINE forNeighbor4_ #-}

forNeighbor8_ :: (Applicative f) => Int -> Int -> Int -> Int -> (Int -> Int -> f ()) -> f ()
forNeighbor8_ h w x y f =
  when
    (x /= 0)
    ( when (y /= 0) (f (x - 1) (y - 1))
        *> f (x - 1) y
        *> when (y /= w - 1) (f (x - 1) (y + 1))
    )
    *> ( when (y /= 0) (f x (y - 1))
          *> when (y /= w - 1) (f x (y + 1))
       )
    *> when
      (x /= h - 1)
      ( when (y /= 0) (f (x + 1) (y - 1))
          *> f (x + 1) y
          *> when (y /= w - 1) (f (x + 1) (y + 1))
      )
{-# INLINE forNeighbor8_ #-}

binarySearchM :: (Monad m) => Int -> Int -> (Int -> m Bool) -> m Int
binarySearchM low0 high0 p = go low0 high0
  where
    go !low !high
      | high <= low = return high
      | otherwise = p mid >>= bool (go (mid + 1) high) (go low mid)
      where
        mid = low + unsafeShiftRL (high - low) 1
{-# INLINE binarySearchM #-}

binarySearch :: Int -> Int -> (Int -> Bool) -> Int
binarySearch low high p = runIdentity $ binarySearchM low high (return . p)
{-# INLINE binarySearch #-}

encode32x2 :: Int -> Int -> Int
encode32x2 x y = unsafeShiftL x 32 .|. y
{-# INLINE encode32x2 #-}

decode32x2 :: Int -> (Int, Int)
decode32x2 xy =
  let !x = unsafeShiftRL xy 32
      !y = xy .&. 0xffffffff
   in (x, y)
{-# INLINE decode32x2 #-}

runSolver :: (a -> IO ()) -> PrimParser a -> IO ()
runSolver = withInputHandle stdin
