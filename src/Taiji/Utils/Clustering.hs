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
import qualified Data.IntMap.Strict as I
import Data.Hashable (Hashable)
import Bio.Data.Bed
import Control.Arrow (first, (&&&))
import Conduit
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
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
        mapC (U.fromList . map readDouble . B.split '\t') .| sinkList
    gen <- create
    subsample <- I.fromList . V.toList . V.take 5000 <$> uniformShuffle (V.fromList $ zip [0..] points) gen
    let sil = silhouette $ flip map clusters $ \xs -> flip mapMaybe xs $ \x -> I.lookup x subsample
    return (length clusters, sil, read stability)

optimalParam :: FilePath
             -> [(Double, (Int, Double, Double))]
             -> IO Double
optimalParam output input = do
    savePlots output [] plt
    return $ fst $ maximumBy (comparing (^._2._2)) $ filter (\x -> x^._2._3 >= 0.9) input
  where
    (res, dat) = unzip $ flip map input $ \(r, (n, sil, stab)) -> (r, (fromIntegral n, sil, stab))
    (num, sils, stabs) = unzip3 dat
    plt = map (setDim 400 300 . addAttr toolbox)
        [ addAttr (yAxisLabel "number of clusters") $
                addAttr (xAxisLabel "resolution") $
                line' $ zip res num 
        , addAttr (yAxisLabel "silhouette width") $
                addAttr (xAxisLabel "resolution") $
                line' $ zip res sils
        , addAttr (yAxisLabel "stability") $
                addAttr (xAxisLabel "resolution") $
                line' $ zip res stabs ]

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
{-# INLINE silhouette #-}

confusionTable :: (Eq a, Hashable a) => [[a]] -> [[a]] -> [[Int]]
confusionTable inputA inputB = flip map inputA' $ \a -> flip map inputB' $ \b ->
    S.size $ S.intersection a b
  where
    inputA' = map S.fromList inputA
    inputB' = map S.fromList inputB
{-# INLINE confusionTable #-}

batchCorrect :: [Maybe (B.ByteString, Maybe B.ByteString)]  -- ^ label and group
             -> V.Vector (U.Vector Double)    -- ^ Data
             -> IO (V.Vector (U.Vector Double))
batchCorrect batchInfo input = do
    vec <- V.thaw input
    forM_ batchGroups $ \(idx, label) -> when (length (nubSort label) > 1) $ do
        res <- batchAveraging label (map (input V.!) idx)
        forM_ (zip idx res) $ \(i, r) -> VM.unsafeWrite vec i r
    V.unsafeFreeze vec
  where
    batchGroups = map (unzip . map (\(i, (x, _)) -> (i, x))) $
        groupBy ((==) `on` (snd . snd)) $ sortBy (comparing (snd . snd)) $
        mapMaybe (\(i, b) -> maybe Nothing (\x -> Just (i,x)) b) $ zip [0..] batchInfo
{-# INLINE batchCorrect #-}

batchAveraging :: [B.ByteString]    -- ^ Batch labels
               -> [U.Vector Double]    -- ^ Data
               -> IO [U.Vector Double]
batchAveraging labels input = withTempDir Nothing $ \dir -> do
    let labelFl = dir <> "/label.txt"
        matFl = dir <> "/mat.txt"
        tmp = dir <> "/tmp.txt"
    B.writeFile labelFl $ B.unlines labels
    B.writeFile matFl $ B.unlines $ map (B.intercalate "\t" . map toShortest . U.toList) input
    shelly $ run_ "taiji-utils" ["correct", "--label", T.pack labelFl, T.pack matFl, T.pack tmp]
    map (U.fromList . map readDouble . B.words) . B.lines <$> B.readFile tmp
{-# INLINE batchAveraging #-}

{-
batchCorrect :: [] -> FilePath -> FilePath -> IO ()
batchCorrect batches spec dir = do
    groups <- getGroups . map (fst . B.breakEnd (=='+')) . B.lines <$> B.readFile rownames
    corrected <- forM groups $ \grp -> withTempDir Nothing $ \tmp -> do
        (label, dat) <- readMatrix spec grp
        let labelFl = tmp <> "/" <> "label.txt"
            matFl = tmp <> "/" <> "ori_mat.txt"
        writeFile labelFl $ unlines label
        B.writeFile matFl $ B.unlines $ map (B.intercalate "\t" . map toShortest) dat
        let output1 = dir <> "/" <> tissueName <> "_new_mat.gz"
            output2 = dir <> "/" <> tissueName <> "_umap.txt"
            tissueName = T.unpack $ T.init $ fst $ T.breakOnEnd "_" $ T.pack $ head label
        shelly $ run_ "python3" ["batchCorrect.py", T.pack matFl, T.pack labelFl, T.pack output1, T.pack output2]
        return (tissueName, fst $ unzip grp, output1, output2)
    let output = dir <> "/corrected.mat.gz"
    dat <- runResourceT $ runConduit $ sourceFile spec .| multiple ungzip .|
        linesUnboundedAsciiC .| sinkVector :: IO (V.Vector B.ByteString)
    mat <- V.unsafeThaw dat
    new <- forM corrected $ \(_, idx, matFl, _) -> do
        mat <- runResourceT $ runConduit $ sourceFile matFl .|
            multiple ungzip .| linesUnboundedAsciiC .| sinkList
        return $ zip idx mat
    forM_ (concat new) $ \(i, xs) -> VM.unsafeWrite mat i xs
    res <- V.toList <$> V.unsafeFreeze mat
    runResourceT $ runConduit $ yieldMany res .| unlinesAsciiC .| gzip .| sinkFile output
  where
    getGroups :: [B.ByteString] -> [[(Int, B.ByteString)]]
    getGroups = filter g . groupBy ((==) `on` f) . sortBy (comparing f) . zip [0..]
      where
        f = fst . B.breakEnd (=='_') . snd
        g x = let m = map snd $ M.toList $ M.fromListWith (+) $ zip (map snd x) $ repeat 1
              in length m > 1 && all (>20) m
    readMatrix :: FilePath -> [(Int, B.ByteString)] -> IO ([String], [[Double]])
    readMatrix input label = do
        dat <- runResourceT $ runConduit $ sourceFile input .| multiple ungzip .|
            linesUnboundedAsciiC .| sinkVector :: IO (V.Vector B.ByteString)
        return (map B.unpack labels, map (\i -> map readDouble $ B.split '\t' $ dat V.! i) idx)
      where
        (idx, labels) = unzip label
-}