{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Taiji.Utils.Matrix
    ( SpMatrix(..)
    , Row
    , mkSpMatrix
    , mkSpMatrixMM
    , transformation
    , saveMatrix
    , saveMatrixMM
    , streamRows
    , sinkRows
    , sinkRows'
    , sampleRows
    , decodeRowWith
    , encodeRowWith
    , colSum
    , meanVariance

    , mapRows
    , deleteCols
    , deleteRows
    , filterCols
    , concatMatrix
    , concatMatrix'
    , mergeMatrices
    ) where

import Data.Conduit.Zlib (multiple, ungzip, gzip)
import Data.ByteString.Lex.Integral (packDecimal)
import Control.Arrow (first, second)
import Data.Conduit.Internal (zipSinks, zipSources)
import qualified Data.Conduit.List as L (groupBy)
import Data.List (foldl', sort)
import           Bio.Utils.Misc          (readInt)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.IntSet as IS
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector as V
import qualified Data.Set as S
import Data.Matrix.Static.Sparse (toTriplet, SparseMatrix(..))
import Data.Matrix.Static.IO (fromMM', toMM)
import Data.Matrix.Dynamic (fromTriplet, Dynamic(..))
import System.Random.MWC.Distributions (uniformPermutation)
import System.Random.MWC
import Text.Printf (printf)

import Taiji.Prelude

data SpMatrix a where
    SpMatrix :: { _num_row  :: Int
                , _num_col  :: Int
                , _source   :: s
                , _streamer :: s -> ConduitT () (Row a) (ResourceT IO) () }
             -> SpMatrix a

instance Functor SpMatrix where
    fmap f SpMatrix{..} = SpMatrix _num_row _num_col _source $ \s -> 
        _streamer s .| mapC (second $ map $ second f)

type Row a = (B.ByteString, [(Int, a)])

mkSpMatrix :: (B.ByteString -> a)   -- ^ Element decoder
           -> FilePath -> IO (SpMatrix a)
mkSpMatrix f input = do
    header <- runResourceT $ runConduit $ sourceFile input .| multiple ungzip .|
        linesUnboundedAsciiC .| headC
    case header of
        Nothing -> error "empty file"
        Just x -> do
            let [n, m] = map (read . T.unpack . T.strip) $ T.splitOn "x" $
                    last $ T.splitOn ":" $ T.pack $ B.unpack x
            return $ SpMatrix n m input decodeSpMatrix
  where
    decodeSpMatrix x = sourceFile x .| multiple ungzip .|
        linesUnboundedAsciiC .| (headC >> mapC (decodeRowWith f))
{-# INLINE mkSpMatrix #-}

-- | Create matrix from matrix market format
mkSpMatrixMM :: FilePath
             -> V.Vector B.ByteString  -- ^ Barcodes
             -> IO (SpMatrix Int)
mkSpMatrixMM fl barcodes = do
    [m,n,_] <- fmap (B.words . fromJust) $ runResourceT $ runConduit $ sourceFile fl .|
        multiple ungzip .| linesUnboundedAsciiC .|
        filterC (not . (=='%') . B.head) .| headC
    return $ SpMatrix (readInt n) (readInt m) fl $ \x -> do
        Dynamic mat@(SparseMatrix _ _ _) <- sourceFile x .| multiple ungzip .| fromMM'
        toTriplet (mat :: SparseMatrix _ _ U.Vector Int) .|
            L.groupBy ((==) `on` (^._2)) .| mapC f
  where
    f xs = (barcodes V.! (head xs ^. _2), map (\(i,_,x) -> (i,x)) xs)
{-# INLINE mkSpMatrixMM #-}

transformation :: ConduitT (Row a) (Row b) (ResourceT IO) ()
               -> SpMatrix a
               -> SpMatrix b
transformation f SpMatrix{..} = SpMatrix _num_row _num_col _source $ \s -> 
    _streamer s .| f
{-# INLINE transformation #-}

saveMatrix :: FilePath
           -> (a -> B.ByteString)
           -> SpMatrix a
           -> IO ()
saveMatrix output f mat = runResourceT $ runConduit $ streamRows mat .|
    sinkRows (_num_row mat) (_num_col mat) f output
{-# INLINE saveMatrix #-}

saveMatrixMM :: FilePath
             -> SpMatrix Int
             -> IO ()
saveMatrixMM output mat = do
    vec <- runResourceT $ runConduit $
        zipSources (iterateC succ 0) (streamRows mat) .| 
        concatMapC f .| sinkVector :: IO (U.Vector (Int,Int,Int))
    case fromTriplet (_num_col mat, _num_row mat) vec of
        Dynamic m -> runResourceT $ runConduit $
            toMM (m :: SparseMatrix _ _ U.Vector Int) .|
            gzip .| sinkFile output
  where
    f (i, (_, xs)) = map (\(j,x) -> (j,i,x)) xs
{-# INLINE saveMatrixMM #-}

streamRows :: SpMatrix a -> ConduitT () (Row a) (ResourceT IO) ()
streamRows SpMatrix{..} = _streamer _source
{-# INLINE streamRows #-}

mapRows :: (Row a -> Row b)
        -> SpMatrix a
        -> SpMatrix b
mapRows f SpMatrix{..} = SpMatrix _num_row _num_col _source $ \s -> 
    _streamer s .| mapC f
{-# INLINE mapRows #-}

sinkRows :: Int   -- ^ Number of rows
         -> Int   -- ^ Number of cols
         -> (a -> B.ByteString) 
         -> FilePath
         -> ConduitT (Row a) Void (ResourceT IO) ()
sinkRows n m encoder output = do
    (l, _) <- (yield header >> mapC (encodeRowWith encoder)) .| zipSinks lengthC sink
    when (l /= n + 1) $ error "incorrect number of rows"
  where
    header = B.pack $ printf "Sparse matrix: %d x %d" n m
    sink = unlinesAsciiC .| gzip .| sinkFile output
{-# INLINE sinkRows #-}

sinkRows' :: Int   -- ^ Number of cols
          -> (a -> B.ByteString) 
          -> FilePath
          -> ConduitT (Row a) Void (ResourceT IO) ()
sinkRows' m encoder output = do
    (n, tmp) <- mapC (encodeRowWith encoder) .| zipSinks lengthC sink
    sourceFile tmp .| linesUnboundedAsciiC .| mapC (decodeRowWith id) .|
        sinkRows n m id output
  where
    sink = unlinesAsciiC .| sinkTempFile "./" "tmp" 
{-# INLINE sinkRows' #-}

  
decodeRowWith :: (B.ByteString -> a) -> B.ByteString -> Row a
decodeRowWith decoder x = (nm, map f values)
  where
    (nm:values) = B.split '\t' x
    f v = let [i, a] = B.split ',' v
          in (readInt i, decoder a)
{-# INLINE decodeRowWith #-}

encodeRowWith :: (a -> B.ByteString) -> Row a -> B.ByteString
encodeRowWith encoder (nm, xs) = B.intercalate "\t" $ nm : map f xs
  where
    f (i,v) = fromJust (packDecimal i) <> "," <> encoder v
{-# INLINE encodeRowWith #-}

colSum :: (Num a, U.Unbox a)
       => SpMatrix a
       -> IO (U.Vector a) 
colSum mat = do
    vec <- UM.replicate (_num_col mat) 0
    runResourceT $ runConduit $ streamRows mat .| mapM_C (f vec)
    U.unsafeFreeze vec
  where
    f vec (_, xs) = forM_ xs $ \(i, x) -> UM.unsafeModify vec (+x) i
{-# INLINE colSum #-}

meanVariance :: SpMatrix Double
             -> IO (U.Vector (Double, Double)) 
meanVariance mat = do
    vec <- UM.replicate (_num_col mat) (0,0)
    runResourceT $ runConduit $ streamRows mat .| mapM_C (f vec)
    vec' <- U.unsafeFreeze vec
    let n = fromIntegral $ _num_row mat
    return $ U.map (\(v, v2) -> let m = v / n in (m, v2 / n - m * m)) vec'
  where
    f vec (_, xs) = forM_ xs $ \(i, x) -> do
        let x' = x / s
        UM.unsafeModify vec (\(v, v2) -> (v + x', v2 + x' * x')) i
      where
        s = foldl1' (+) (map snd xs) / 10000
{-# INLINE meanVariance #-}

deleteCols :: [Int]      -- ^ Columns to be removed
           -> SpMatrix a
           -> SpMatrix a
deleteCols idx mat = (mapRows (second changeIdx) mat){_num_col = _num_col mat - length idx}
  where
    changeIdx = filter ((>=0) . fst) . map (first (newIdx U.!))
    idx' = S.fromList idx
    newIdx = U.unfoldr f (0,0)
    f (i, acc) | i >= _num_col mat = Nothing
               | i `S.member` idx' = Just (-1, (i+1, acc))
               | otherwise = Just (acc, (i+1, acc+1))
{-# INLINE deleteCols #-}

deleteRows :: [Int]  -- ^ Rows to be removed
           -> SpMatrix a -> SpMatrix a
deleteRows idx SpMatrix{..} = SpMatrix (_num_row - length idx) _num_col _source $ \s ->
    zipSources (iterateC succ 0) (_streamer s) .| filterC f .| mapC snd
  where
    f (i,_) = not $ i `IS.member` idx'
    idx' = IS.fromList idx
{-# INLINE deleteRows #-}

sampleRows :: PrimMonad m
           => Int -> SpMatrix a -> Gen (PrimState m) -> m (SpMatrix a)
sampleRows n mat gen
    | n >= _num_row mat = return mat
    | otherwise = do
        toRemove <- U.toList . U.take (_num_row mat - n) <$>
            uniformPermutation (_num_row mat) gen
        return $ deleteRows toRemove mat
{-# INLINE sampleRows #-}

filterCols :: FilePath   -- ^ New matrix
           -> [Int]      -- ^ Columns to be removed
           -> FilePath   -- ^ Old matrix
           -> IO ()
filterCols output idx input = do
    mat <- mkSpMatrix id input
    let header = B.pack $ printf "Sparse matrix: %d x %d" (_num_row mat) (_num_col mat - length idx)
        newIdx = U.fromList $ zipWith (-) [0 .. _num_col mat - 1] $ go (-1,0) (sort idx)
        f = map (first (newIdx U.!)) . filter (not . (`S.member` idx') . fst)
        idx' = S.fromList idx
    runResourceT $ runConduit $ streamRows mat .| mapC (second f) .|
        (yield header >> mapC (encodeRowWith id)) .| unlinesAsciiC .|
        gzip .| sinkFile output
  where
    go (prev, c) (i:x) = replicate (i-prev) c ++ go (i, c+1) x
    go (_, c) [] = repeat c

concatMatrix' :: [SpMatrix a]
              -> SpMatrix a
concatMatrix' mats
    | any (/=nCol) (map _num_col mats) = error "Column unmatched!"
    | otherwise = SpMatrix nRow nCol mats $ mapM_ streamRows
  where
    nRow = sum $ map _num_row mats
    nCol = _num_col $ head mats
{-# INLINE concatMatrix' #-}

-- | Combine rows of matrices. The matrices should have same column names.
concatMatrix :: FilePath   -- ^ Output merged matrix
             -> [(Maybe B.ByteString, FilePath)] -- ^ A list of matrix
             -> IO ()
concatMatrix output inputs = do
    mats <- forM inputs $ \(nm, fl) -> do
        mat <- mkSpMatrix id fl
        return (nm, mat)
    runResourceT $ runConduit $ merge mats .| sinkFile output
  where
    merge mats
        | any (/=nBin) (map (_num_col . snd) mats) = error "Column unmatched!"
        | otherwise = source .| (yield header >> mapC (encodeRowWith id)) .|
            unlinesAsciiC .| gzip
      where
        source = forM_ mats $ \(nm, mat) ->
            let f x = case nm of
                    Nothing -> x
                    Just n -> n <> "+" <> x
            in streamRows mat .| mapC (first f)
        nCell = foldl' (+) 0 $ map (_num_row . snd) mats
        nBin = _num_col $ snd $ head mats
        header = B.pack $ printf "Sparse matrix: %d x %d" nCell nBin

mergeMatrices :: [(V.Vector B.ByteString, SpMatrix a)]
              -> (V.Vector B.ByteString, SpMatrix a)
mergeMatrices mats = ( V.fromList $ S.toList idxSet
                     , SpMatrix nRow nCol (idxSet, mats) fun )
  where
    fun (idx, ms) = forM_ ms $ \(names, mat) -> do
        let newIdx = V.map (\x -> S.findIndex x idx) names
        streamRows mat .| mapC (second (map (first (newIdx V.!))))
    nRow = sum $ map (_num_row . snd) mats
    nCol = S.size idxSet
    idxSet = runIdentity $ runConduit $
        mapM_ (V.mapM_ yield . fst) mats .| foldlC (flip S.insert) S.empty
{-# INLINE mergeMatrices #-}
