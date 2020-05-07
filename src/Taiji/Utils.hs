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

    , computeRAS
    , computeSS
    , computeCDF

    -- * Plotting
    , visualizeCluster
    , sampleCells

    , module Taiji.Utils.Matrix
    ) where

import Data.BBI.BigBed
import Bio.Data.Experiment
import Bio.Data.Bed
import Bio.Utils.Functions (scale)
import Control.Arrow (first)
import           Data.CaseInsensitive    (mk)
import Data.List (foldl')
import           Bio.Utils.Misc          (readDouble, readInt)
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
import Statistics.Sample (varianceUnbiased, mean)

import Taiji.Utils.Plot.ECharts
import Taiji.Utils.Matrix
import qualified Taiji.Utils.DataFrame as DF
import Taiji.Prelude

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
    normalize xs = V.map (/s) xs'
      where
        s = V.sum xs'
        xs' = V.map (+pseudoCount) xs
        pseudoCount = 1
{-# INLINE computeSS #-}

computeCDF :: DF.DataFrame Double -> IO (V.Vector Double, Double, Double)
computeCDF df = do
    gen <- create
    let std = let rows = filter ((>5) . V.maximum) $ Mat.toRows $
                      Mat.map (+1) $ DF._dataframe_data df
                  f xs = (xs, 1 / entropy (normalize xs))
                  n = truncate $ (0.7 :: Double) * fromIntegral (length rows)
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

visualizeCluster :: [CellCluster] -> [EChart]
visualizeCluster cls =
    [ addAttr toolbox $ scatter' $ flip map cls $ \(CellCluster nm cells) ->
        (B.unpack nm, map _cell_2d cells) ] ++ if noName then [] else
          [ addAttr toolbox $ scatter' $ map (first head . unzip) $
              groupBy ((==) `on` fst) $ sortBy (comparing fst) $ concatMap
              (map (\x -> (getName $ _cell_barcode x, _cell_2d x)) . _cluster_member) cls
          ]
  where
    noName = null $ getName $ _cell_barcode $ head $
        _cluster_member $ head cls
    getName x = let prefix = fst $ B.breakEnd (=='+') x
                in if B.null prefix then "" else B.unpack $ B.init prefix

-- | Random sample 30,000 cells.
sampleCells :: [CellCluster] -> IO [CellCluster]
sampleCells clusters
    | ratio >= 1 = return clusters
    | otherwise = do
        gen <- create
        forM clusters $ \c -> do
            s <- sampling gen ratio $ V.fromList $ _cluster_member c
            return $ c {_cluster_member = V.toList s}
  where
    n = foldl1' (+) $ map (length . _cluster_member) clusters
    ratio = 1 / (fromIntegral n / 30000) :: Double
    sampling gen frac v = V.take n' <$> uniformShuffle v gen
      where
        n' = max 200 $ truncate $ frac * fromIntegral (V.length v)