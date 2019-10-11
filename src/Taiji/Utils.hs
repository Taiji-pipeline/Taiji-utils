{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module Taiji.Utils
    ( BBIndex
    , openBBs
    , queryBB
    , readExpression
    , lp

      -- * Sparse Matrix
    , SpMatrix(..)
    , Row
    , mkSpMatrix
    , streamRows
    , sinkRows
    , decodeRowWith
    , encodeRowWith

    , filterCols
    , concatMatrix

    ) where

import Data.BBI.BigBed
import Bio.Data.Experiment
import Bio.Data.Bed
import Bio.Utils.Functions (scale)
import Data.Conduit.Zlib (multiple, ungzip, gzip)
import Conduit
import Data.Maybe
import Lens.Micro ((^.))
import Data.ByteString.Lex.Integral (packDecimal)
import Control.Arrow (first, second)
import Data.Conduit.Internal (zipSinks)
import           Data.CaseInsensitive    (mk)
import Data.List (foldl', sort)
import Control.Monad
import           Bio.Utils.Misc          (readDouble, readInt)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Matrix.Unboxed as MU
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed as U
import qualified Data.Set as S
import Text.Printf (printf)

import Taiji.Types

-- | Open bigbed files.
openBBs :: [(B.ByteString, Maybe (File '[] 'BigBed))]
        -> IO BBIndex
openBBs xs = fmap (HM.fromList . catMaybes) $ forM xs $ \(chr, x) -> case x of
    Nothing -> return Nothing
    Just fl -> do
        bb <- openBBedFile $ fl^.location
        return $ Just (chr, bb)

queryBB :: BEDLike b => b -> BBIndex -> ConduitT () TFBS IO ()
queryBB bed idx = case HM.lookup (bed^.chrom) idx of
    Nothing -> return ()
    Just i -> query (bed^.chrom, bed^.chromStart, bed^.chromEnd) i .| mapC f
  where
    f (chr, s, e, rest) = BEDExt (asBed chr s e) info
      where
        info = SiteInfo
            { _tf_name = mk $ head $ B.split '+' f1
            , _site_affinity = toSiteAffinity $ readInt f2
            , _peak_affinity = toPeakAffinity 100 }
        (f1:f2:_) = B.split '\t' rest

lp :: Int -> [Double] -> Double
lp p = (**(1/fromIntegral p)) . foldl' (+) 0 . map (**fromIntegral p)
{-# INLINE lp #-}

-- | Read RNA expression data
readExpression :: Double    -- ^ Threshold to call a gene as non-expressed
               -> B.ByteString  -- ^ cell type
               -> FilePath
               -> IO (HM.HashMap GeneName (Double, Double)) -- ^ absolute value and z-score
readExpression cutoff ct fl = do
    c <- B.readFile fl
    let ((_:header):dat) = map (B.split '\t') $ B.lines c
        rowNames = map (mk . head) dat
        dataTable = map (U.fromList . map ((+pseudoCount) . readDouble) . tail) dat
        dataTable' = MU.fromRows $ zipWith U.zip dataTable $
            map computeZscore dataTable :: MU.Matrix (Double, Double)
        idx = fromMaybe (error $ "Cell type:" ++ B.unpack ct ++ " not found!") $
            lookup ct $ zip header [0..]
    return $ HM.fromList $ zip rowNames $ U.toList $ dataTable' `MU.takeColumn` idx
  where
    pseudoCount = 0.1
    computeZscore xs
        | U.length xs == 1 = U.map log xs
        | U.all (<cutoff) xs = U.replicate (U.length xs) 0
        | U.length xs == 2 = let fc = log $ U.head xs / U.last xs
                             in U.fromList [fc, negate fc]
        | U.all (== U.head xs) xs = U.replicate (U.length xs) 0
        | otherwise = scale xs
{-# INLINE readExpression #-}

-------------------------------------------------------------------------------
-- Sparse Matrix
-------------------------------------------------------------------------------

data SpMatrix a = SpMatrix
    { _num_row :: Int
    , _num_col :: Int
    , _filepath :: FilePath
    , _decoder :: FilePath -> ConduitT () (Row a) (ResourceT IO) ()
    }

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
{-# NOINLINE mkSpMatrix #-}

streamRows :: SpMatrix a -> ConduitT () (Row a) (ResourceT IO) ()
streamRows sp = (_decoder sp) (_filepath sp)

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

-- | Combine rows of matrices. 
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
