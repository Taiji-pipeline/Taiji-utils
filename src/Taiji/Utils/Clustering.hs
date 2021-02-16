{-# LANGUAGE OverloadedStrings #-}
module Taiji.Utils.Clustering where

import qualified Data.ByteString.Char8 as B
import Data.Binary (encodeFile, decodeFile)
import Bio.Utils.Functions (scale)
import Bio.Utils.Misc
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable)
import Bio.Data.Bed
import Control.Arrow (first, (&&&))
import Conduit
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Unboxed as U
import System.IO
import Data.List.Ordered (nubSort)
import Shelly (shelly, run_, escaping)
import Data.Conduit.Zlib (multiple, ungzip)
import AI.Clustering.Hierarchical.Types (computeDists, (!))
import AI.Clustering.Hierarchical (euclidean)
import Statistics.Sample (varianceUnbiased, mean)
import System.Random.MWC.Distributions
import System.Random.MWC

import Taiji.Prelude
import Taiji.Utils.Plot.ECharts
import Taiji.Utils.Plot
   
-- | Evaluating the clustering results
evalClusters :: Optimizer  -- ^ Optimizer
             -> Double     -- ^ Resolution
             -> FilePath   -- ^ spectral
             -> FilePath   -- ^ knn
             -> IO (Int, Double, Double)
evalClusters optimizer res spectral knn = withTemp Nothing $ \tmpFl -> do
    shelly $ run_ "taiji-utils" [ "clust", T.pack knn, T.pack tmpFl
        , "--stability", "--res", T.pack $ show res, "--optimizer"
        , case optimizer of
            RBConfiguration -> "RB"
            CPM -> "CPM"
        ]
    [_, stability] <- words . head . lines <$> readFile tmpFl
    shelly $ run_ "taiji-utils" [ "clust", T.pack knn, T.pack tmpFl
        , "--res", T.pack $ show res, "--optimizer"
        , case optimizer of
            RBConfiguration -> "RB"
            CPM -> "CPM"
        ]
    clusters <- map (map readInt . B.split ',') . B.lines <$> B.readFile tmpFl
    points <- runResourceT $ runConduit $ sourceFile spectral .|
        multiple ungzip .| linesUnboundedAsciiC .|
        mapC (U.fromList . map readDouble . B.split '\t') .| sinkVector
    let sil = silhouette $ (map . map) (points V.!) clusters
    return (length clusters, sil, read stability)

optimalParam :: FilePath
             -> [((Optimizer, Double), (Int, Double, Double))]
             -> IO (Optimizer, Double)
optimalParam output input = do
    savePlots output [] plt
    return $ fst $ maximumBy (comparing (^._2._2)) $ filter (\x -> x^._2._3 >= 0.9) input
  where
    (res, dat) = unzip $ flip map input $ \((_, r), (n, sil, stab)) -> (r, (fromIntegral n, sil, stab))
    (num, sils, stabs) = unzip3 dat
    plt = [ scatter' [("number of clusters", zip res num)]
          , scatter' [("Silhouette", zip res sils)]
          , scatter' [("Stability", zip res stabs)] ]

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

silhouette :: [[U.Vector Double]]   -- ^ Groups of objects
           -> Double
silhouette input
    | length input <= 1 = 0
    | otherwise = mean $ U.fromList $ flip concatMap clusters $ \(i, cl) ->
        let cls = map snd $ filter ((/= i) . fst) clusters
        in flip map cl $ \x ->
            let a = getD x cl
                b = minimum $ map (getD x) cls
            in (b - a) / max a b
  where
    getD i cl = mean $ U.fromList $ map (\j -> distMat ! (i, j)) $ filter (/= i) cl
    clusters = zip [0 :: Int ..] $ go 0 input
      where
        go i (x:xs) = let n = length x in take n [i..] : go (i + n) xs
        go _ _ = []
    distMat = computeDists euclidean $ V.fromList $ concat input

confusionTable :: (Eq a, Hashable a) => [[a]] -> [[a]] -> [[Int]]
confusionTable inputA inputB = flip map inputA' $ \a -> flip map inputB' $ \b ->
    S.size $ S.intersection a b
  where
    inputA' = map S.fromList inputA
    inputB' = map S.fromList inputB