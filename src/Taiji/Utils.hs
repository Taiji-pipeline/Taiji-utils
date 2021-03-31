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

    , readGenesValidated

    , computeRAS
    , computeSS
    , computeCDF

    , module Taiji.Utils.Matrix
    , module Taiji.Utils.Clustering
    ) where

import Data.BBI.BigBed
import Bio.Data.Experiment
import Bio.Data.Bed
import Bio.Utils.Functions (scale)
import           Data.CaseInsensitive    (mk)
import qualified Data.HashMap.Strict as HM
import qualified Data.Matrix.Unboxed as MU
import qualified Data.Matrix as Mat
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import System.Random.MWC.Distributions
import System.Random.MWC
import Statistics.Sample (mean, varianceUnbiased)
import Bio.RealWorld.GENCODE (readGenes, Gene(..))

import Taiji.Utils.Clustering
import Taiji.Utils.Matrix
import qualified Taiji.Utils.DataFrame as DF
import Taiji.Prelude

readGenesValidated :: FilePath -> IO [Gene]
readGenesValidated = fmap validate . readGenes
  where
    validate xs
        | null xs = error "No gene was found in the annotation file"
        | all (null . geneTranscripts) xs = error "No transcript was found in the annotation file"
        | otherwise = xs
{-# INLINE readGenesValidated #-}

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
    f (chr, s, e, rest) = BEDExt (asBed chr s e) site
      where
        site = SiteInfo
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
readExpression thres ct fl = do
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
        | U.all (<thres) xs = U.replicate (U.length xs) 0
        | U.length xs == 2 = let fc = log $ U.head xs / U.last xs
                             in U.fromList [fc, negate fc]
        | U.all (== U.head xs) xs = U.replicate (U.length xs) 0
        | otherwise = scale xs
{-# INLINE readExpression #-}

computeRAS :: FilePath -> IO (U.Vector Double)
computeRAS fl = do
    mat <- mkSpMatrix readInt fl
    fmap accScore $ runResourceT $ runConduit $
        streamRows mat .| sink (_num_col mat)
  where
    sink n = do
        v <- lift $ UM.replicate n 0.1
        mapM_C $ \(_, xs) -> forM_ xs $ \(i, x) ->
            UM.unsafeModify v (+fromIntegral x) i
        lift $ U.unsafeFreeze v
    accScore xs = U.map (\x -> x * 1000000 / s) xs
      where
        s = U.sum xs

-- | Compute Specificity Score (SS).
computeSS :: DF.DataFrame Double -> DF.DataFrame Double
computeSS df = df{ DF._dataframe_data = mat}
  where
    mat = Mat.fromRows $ map (q_t . normalize) $ Mat.toRows $
        DF._dataframe_data df
    q_t xs = V.map (\x -> e - logBase 2 x) xs
      where
        e = negate $ V.sum $ V.map (\p -> p * logBase 2 p) xs
    normalize xs = V.map (/s) xs
      where
        s = V.sum xs
{-# INLINE computeSS #-}

computeCDF :: DF.DataFrame Double -> IO (V.Vector Double, Double, Double)
computeCDF df = do
    gen <- create
    let std = let rows = filter ((>1) . V.maximum) $ Mat.toRows $ DF._dataframe_data df
                  f xs = (xs, 1 / entropy (normalize xs))
                  n = truncate $ (0.3 :: Double) * fromIntegral (length rows)
                  getFold xs = let m = mean xs in V.map (logBase 2 . (/m)) xs
                  entropy xs = negate $ V.sum $ V.map (\p -> p * logBase 2 p) xs
              in sqrt $ varianceUnbiased $ V.concat $ map (getFold . fst) $
                  take n $ sortBy (comparing snd) $ map f $ rows
    runConduit $ replicateMC nSample (V.toList <$> mkSample gen std) .|
        concatC .| mkCDF 0.001
  where
    ncol = Mat.cols $ DF._dataframe_data df
    nSample = 500000
    mkCDF res = do
        vec <- VM.replicate n 0
        mapM_C $ \x -> do
          let i = min (n-1) $ truncate $ x / res
          VM.unsafeModify vec (+1) i
        v <- V.scanl1' (+) <$> V.unsafeFreeze vec
        return (V.map (/(V.last v)) v, res, fromIntegral $ nSample*ncol)
      where
        n = truncate $ 2 * logBase 2 (fromIntegral ncol) / res
    -- | https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2396180/
    normalize xs = V.map (/s) xs
      where
        s = V.sum xs
    mkSample gen !std = do
        folds <- replicateM ncol $ normal 0 std gen 
        return $ q_t $ normalize $ V.fromList $ map (\x -> 2**x) folds
    q_t xs = V.map (\x -> e - logBase 2 x) xs
      where
        e = negate $ V.sum $ V.map (\p -> p * logBase 2 p) xs
{-# INLINE computeCDF #-}