{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
module Taiji.Utils.Clustering
    ( evalClusters
    , computeClusterMetrics
    , computeReproducibility
    , readKNNGraph
    , optimalParam
    , visualizeCluster
    , sampleCells
    , silhouette
    , confusionTable
    , batchCorrect
    , leiden
    , ari
    ) where

import qualified Data.ByteString.Char8 as B
import Data.Binary (encodeFile, decodeFile)
import Bio.Utils.Misc
import qualified Data.Text as T
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable)
import Control.Arrow (first, second)
import Conduit
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import Data.List.Ordered (nubSort)
import Shelly (shelly, run_)
import Data.Conduit.Zlib (multiple, ungzip)
import AI.Clustering.Hierarchical.Types (computeDists, (!))
import AI.Clustering.Hierarchical (euclidean)
import Statistics.Sample (mean)
import System.Random.MWC.Distributions
import Language.Javascript.JMacro
import System.Random.MWC

import IGraph.Random
import IGraph
import qualified IGraph.Algorithms.Community as I

import Taiji.Prelude
import Taiji.Utils.Plot.ECharts
import Taiji.Utils.Plot
   
-- | Evaluating the clustering results
evalClusters :: FilePath
             -> Optimizer  -- ^ Optimizer
             -> Double     -- ^ Resolution
             -> FilePath   -- ^ knn
             -> IO ([FilePath], FilePath)
evalClusters dir optimizer res knn = do
    gr <- readKNNGraph knn
    gen <- create

    perturbed <- forM [1::Int ..5] $ \i -> do
        let output' = dir <> "/perturbed_clustering_result_" <> show i <> ".bin"
        r <- mutateGraph gr gen >>= leiden res optimizer 
        encodeFile output' $ map snd $ sort $ concat $ zipWith (\a b -> zip a $ repeat b) r [0::Int ..]
        return output'

    let output = dir <> "/clustering_result.bin"
    (filter ((>100) . length) <$> leiden res optimizer gr) >>= encodeFile output
    return (perturbed, output)
  where
    mutateGraph gr gen = do
        let n = truncate $ fromIntegral (nEdges gr) * (0.02 :: Double)
        xs <- take n . V.toList <$> uniformShuffle (V.fromList $ edges gr) gen
        return $ delEdges xs gr

computeReproducibility :: [[Int]] -- ^ cluster
                       -> [[Int]]  -- ^ perturbed clusters
                       -> [Double]
computeReproducibility a bs = map (mean . U.fromList) $ transpose $ flip map bs $ \b ->
    let b' = U.fromList b
    in map (reproducibility . map (b' U.!)) a

computeClusterMetrics :: ([FilePath], FilePath)
                      -> FilePath
                      -> IO (Int, Double, Double)
computeClusterMetrics (perturbed, cl) coordinate = do
    stability <- (mean . U.fromList . map ari . comb) <$> mapM decodeFile perturbed
    clusters <- decodeFile cl
    points <- fmap V.fromList $ runResourceT $ runConduit $ sourceFile coordinate .|
        multiple ungzip .| linesUnboundedAsciiC .|
        mapC (U.fromList . map readDouble . B.split '\t') .| sinkList
    gen <- create

    let samplingRatio = 10000 / fromIntegral (V.length points) :: Double
    clusters' <- forM clusters $ \c -> do
        let n = max 2 $ truncate $ fromIntegral (length c) * samplingRatio
        V.toList . V.take n <$> uniformShuffle (V.fromList c) gen

    let sil = silhouette $ (map . map) (points V.!) clusters'
    return (length clusters, sil, stability)
  where
    comb (x:xs) = zip (repeat x) xs ++ comb xs
    comb _ = []

readKNNGraph :: FilePath -> IO (Graph 'U () Double)
readKNNGraph fl = runResourceT $ runConduit $ sourceFile fl .|
    multiple ungzip .| linesUnboundedAsciiC .| sink
  where
    sink = do
        n <- headC >>= \case
            Nothing -> error ""
            Just x -> return $ readInt x
        es <- mapC f .| sinkList
        return $ mkGraph (replicate n ()) $ replaceInf es
    f x = let (a:b:c:_) = B.split '\t' x
          in ((readInt a, readInt b), readDouble' c)
    readDouble' x | x == "inf" = Nothing
                  | otherwise = Just $ readDouble x
    replaceInf es = let m = maximum $ mapMaybe snd es
                    in map (second (fromMaybe m)) es

optimalParam :: FilePath
             -> [(Double, (Int, Double, Double))]
             -> IO Double
optimalParam output input = do
    savePlots output [] plt
    return optimal
  where
    optimal = fst $ maximumBy (comparing (^._2._2)) $ case filter (\x -> x^._2._3 >= 0.9) input of
        [] -> input
        x -> x
    (res, dat) = unzip $ flip map (sortBy (comparing fst) input) $ \(r, (n, sil, stab)) ->
        (r, (fromIntegral n, sil, stab))
    (num, sils, stabs) = unzip3 dat
    plt = map (setDim 400 300 . addAttr toolbox)
        [ addAttr (yAxisLabel "number of clusters") $
                addAttr (xAxisLabel "resolution") $
                lineplot $ zip res num 
        , addAttr (yAxisLabel "silhouette width") $
                addAttr (xAxisLabel "resolution") $
                lineplot $ zip res sils
        , addAttr (yAxisLabel "stability") $
                addAttr (xAxisLabel "resolution") $
                lineplot $ zip res stabs ]
    lineplot input = mkEChart [jmacroE| {
        grid: { containLabel: true },
        xAxis: {
            type: "value",
            axisLine: {onZero: false}
        },
        tooltip: {
            trigger: "axis"
        }, 
        yAxis: {
            type: "value",
            axisLine: {onZero: false}
        }, 
        series: [{
            data: `dataset`,
            markLine: {symbolSize: 0, precision: 10, data: [{xAxis: `optimal`}]},
            type: "line"
        }]
        } |]
      where
        dataset = map (\(x,y) -> [x,y]) input
    

visualizeCluster :: [CellCluster] -> [EChart]
visualizeCluster cls =
    [ addAttr toolbox $ scatter' $ flip map cls $ \(CellCluster nm cells _) ->
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

leiden :: Double -> Optimizer -> Graph 'U () Double -> IO [[Int]]
leiden resolution optimizer gr = withSeed 9304 $ fmap (sortBy (flip (comparing length))) .
    I.findCommunity gr nodeWeight edgeWeight I.leiden{I._resolution=res}
  where
    edgeWeight
        | optimizer `elem` [RBConfiguration, CPM] = Nothing
        | otherwise = Just id
    (nodeWeight, res) 
        | optimizer `elem` [RBConfiguration, RBConfigurationWeighted] =
            ( Just $ \i _ -> foldl1' (+) $ map (\j -> edgeLab gr (i, j)) $ neighbors gr i
            , resolution / (2 * (foldl1' (+) $ map snd $ labEdges gr)))
        | otherwise = (Nothing, resolution)

-- |
reproducibility :: [Int]   -- membership
                -> Double
reproducibility xs = fromIntegral sameGroup / fromIntegral (c2 $ length xs)
  where
    counts = M.elems $ M.fromListWith (+) $ zip xs $ repeat (1 :: Int)
    sameGroup = foldl1' (+) $ map c2 counts
    c2 x = (x * (x - 1)) `div` 2

-- | Adjusted Rand Index: <http://en.wikipedia.org/wiki/Rand_index>
ari :: ([Int], [Int]) -> Double
ari (a, b) | length a /= length b = error "unequal length"
           | a == b = 1
           | otherwise = ari' $ counts a b
  where
    ari' (Counts cxy cx cy) =  (sum1 - sum2*sum3/choicen2) 
                            / (1/2 * (sum2+sum3) - (sum2*sum3) / choicen2)
      where choicen2 = choice (sum . M.elems $ cx) 2
            sum1 = sum [ choice nij 2 | nij <- M.elems cxy ]
            sum2 = sum [ choice ni 2 | ni <- M.elems cx ]
            sum3 = sum [ choice nj 2 | nj <- M.elems cy ]
    -- | Creates count table 'Counts'
    counts xs = foldl' f mempty . zipWith ((,)) xs
        where f cs@(Counts cxy cx cy) p@(x,y) = 
                cs { joint       = M.insertWith (+) p 1 cxy
                , marginalFst = M.insertWith (+) x 1 cx
                , marginalSnd = M.insertWith (+) y 1 cy }
    -- | The binomial coefficient: C^n_k = PROD^k_i=1 (n-k-i)\/i
    choice :: Double -> Double -> Double
    choice n k = foldl' (*) 1 [n-k+1 .. n] / foldl' (*) 1 [1 .. k]

-- | Count table
data Counts = Counts 
    { joint :: !(M.HashMap (Int, Int) Double) -- ^ Counts of both components
    , marginalFst :: !(M.HashMap Int Double) -- ^ Counts of the first component
    , marginalSnd :: !(M.HashMap Int Double) -- ^ Counts of the second component
    } 

instance Monoid Counts where
    mempty = Counts M.empty M.empty M.empty
    c `mappend` k = 
        Counts { joint = unionPlus (joint c) (joint k)
               , marginalFst = unionPlus (marginalFst c) (marginalFst k)
               , marginalSnd = unionPlus (marginalSnd c) (marginalSnd k)
               }
      where
        unionPlus m = M.foldlWithKey' (\z k' v -> M.insertWith (+) k' v z) m

instance Semigroup Counts where
    c <> k = 
        Counts { joint = unionPlus (joint c) (joint k)
               , marginalFst = unionPlus (marginalFst c) (marginalFst k)
               , marginalSnd = unionPlus (marginalSnd c) (marginalSnd k)
               }
      where
        unionPlus m = M.foldlWithKey' (\z k' v -> M.insertWith (+) k' v z) m
