{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Taiji.Utils.DataFrame
    ( DataFrame(..)
    , DataFrameIndex(..)
    , mkDataFrame
    , fromMatrix
    , isEmpty
    , Taiji.Utils.DataFrame.transpose
    , rbind
    , cbind
    , rowNames
    , colNames
    , ReodrderFn
    , reorderColumns
    , reorderRows
    , Taiji.Utils.DataFrame.map
    , mapRows
    , mapCols
    , filterRows
    , Taiji.Utils.DataFrame.zip
    , Taiji.Utils.DataFrame.unzip
    , readData
    , writeTable
    , readTable
    , readTableWith
    , orderByHClust
    , orderByKMeans
    , orderDataFrame
    
    , pearson
    , spearman
    ) where

import           Bio.Utils.Functions    (ihs')
import           Bio.Utils.Misc         (readDouble)
import qualified Data.ByteString.Char8  as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.CaseInsensitive   as CI
import           Data.Function          (on)
import           Data.List as L
import AI.Clustering.Hierarchical hiding (normalize)
import AI.Clustering.KMeans
import Data.Maybe
import qualified Data.Matrix            as M
import qualified Data.Vector            as V
import qualified Data.HashMap.Strict    as HM
import Statistics.Correlation (pearsonMatByRow, spearmanMatByRow)
import Statistics.Matrix (toRowLists, fromRowLists)

data DataFrame a = DataFrame
    { _dataframe_row_names :: V.Vector T.Text
    , _dataframe_row_names_idx :: HM.HashMap T.Text Int
    , _dataframe_col_names :: V.Vector T.Text
    , _dataframe_col_names_idx :: HM.HashMap T.Text Int
    , _dataframe_data :: M.Matrix a
    } deriving (Show)

mkDataFrame :: [T.Text]     -- ^ row names
            -> [T.Text]     -- ^ col names
            -> [[a]]        -- ^ data
            -> DataFrame a
mkDataFrame r c d = DataFrame
    { _dataframe_row_names = V.fromList r
    , _dataframe_row_names_idx = HM.fromList $ L.zip r [0..]
    , _dataframe_col_names = V.fromList c
    , _dataframe_col_names_idx = HM.fromList $ L.zip c [0..]
    , _dataframe_data = M.fromLists d }

fromMatrix :: [T.Text]     -- ^ row names
           -> [T.Text]     -- ^ col names
           -> M.Matrix a  -- ^ data
           -> DataFrame a
fromMatrix r c mat = DataFrame
    { _dataframe_row_names = V.fromList r
    , _dataframe_row_names_idx = HM.fromList $ L.zip r [0..]
    , _dataframe_col_names = V.fromList c
    , _dataframe_col_names_idx = HM.fromList $ L.zip c [0..]
    , _dataframe_data = mat }

class DataFrameIndex i where
    csub :: DataFrame a -> [i] -> DataFrame a
    cindex :: DataFrame a -> i -> V.Vector a
    rsub :: DataFrame a -> [i] -> DataFrame a
    rindex :: DataFrame a -> i -> V.Vector a
    (!)  :: DataFrame a -> (i,i) -> a

instance DataFrameIndex Int where
    csub df idx = df
        { _dataframe_col_names = V.fromList col_names
        , _dataframe_col_names_idx = HM.fromList $ L.zip col_names [0..]
        , _dataframe_data = M.fromColumns $ L.map (_dataframe_data df `M.takeColumn`) idx }
      where
        col_names = L.map (_dataframe_col_names df V.!) idx

    cindex df idx = _dataframe_data df `M.takeColumn` idx

    rsub df idx = df
        { _dataframe_row_names = V.fromList row_names
        , _dataframe_row_names_idx = HM.fromList $ L.zip row_names [0..]
        , _dataframe_data = M.fromRows $ L.map (_dataframe_data df `M.takeRow`) idx }
      where
        row_names = L.map (_dataframe_row_names df V.!) idx

    rindex df idx = _dataframe_data df `M.takeRow` idx

    (!) df (i,j) = _dataframe_data df M.! (i,j)

instance DataFrameIndex T.Text where
    csub df idx = csub df idx'
      where
        idx' = L.map (\i -> HM.lookupDefault
            (error $ "index doesn't exist: " ++ T.unpack i) i $
            _dataframe_col_names_idx df) idx

    cindex df i = cindex df i'
      where
        i' = HM.lookupDefault (error $ "index doesn't exist: " ++ T.unpack i) i $
            _dataframe_col_names_idx df

    rsub df idx = rsub df idx'
      where
        idx' = L.map (\i -> HM.lookupDefault
            (error $ "index doesn't exist: " ++ T.unpack i) i $
            _dataframe_row_names_idx df) idx

    rindex df i = rindex df i'
      where
        i' = HM.lookupDefault (error $ "index doesn't exist: " ++ T.unpack i) i $
            _dataframe_row_names_idx df

    (!) df (a,b) = (!) df (i,j)
      where
        i = HM.lookupDefault (error $ "index doesn't exist: " ++ T.unpack a) a $
            _dataframe_row_names_idx df
        j = HM.lookupDefault (error $ "index doesn't exist: " ++ T.unpack b) b $
            _dataframe_col_names_idx df

isEmpty :: DataFrame a -> Bool
isEmpty df = r == 0 || c == 0
  where
    (r,c) = M.dim $ _dataframe_data df

rbind :: [DataFrame a] -> DataFrame a
rbind dfs | allTheSame (L.map _dataframe_col_names dfs) = (head dfs)
    { _dataframe_row_names = row_names
    , _dataframe_row_names_idx = HM.fromList $ L.zip (V.toList row_names) [0..]
    , _dataframe_data = M.fromBlocks undefined $ L.map (return . _dataframe_data) dfs }
          | otherwise = error "Columns names differ"
  where
    allTheSame xs = all (== head xs) (tail xs)
    row_names = V.concat $ L.map (_dataframe_row_names) dfs

cbind :: [DataFrame a] -> DataFrame a
cbind dfs | allTheSame (L.map _dataframe_row_names dfs) = (head dfs)
    { _dataframe_col_names = col_names
    , _dataframe_col_names_idx = HM.fromList $ L.zip (V.toList col_names) [0..]
    , _dataframe_data = M.fromBlocks undefined [L.map _dataframe_data dfs] }
          | otherwise = error "Row names differ"
  where
    allTheSame xs = all (== head xs) (tail xs)
    col_names = V.concat $ L.map (_dataframe_col_names) dfs

map :: (a -> b) -> DataFrame a -> DataFrame b
map f df = df{_dataframe_data = M.map f $ _dataframe_data df}

zip :: DataFrame a -> DataFrame b -> DataFrame (a,b)
zip df1 df2
    | _dataframe_col_names df1 == _dataframe_col_names df2 &&
      _dataframe_row_names df1 == _dataframe_row_names df2 = df1
        {_dataframe_data = M.zip (_dataframe_data df1) $ _dataframe_data df2}
    | otherwise = error "names mismatch"

unzip :: DataFrame (a,b) -> (DataFrame a, DataFrame b)
unzip df = (df{_dataframe_data = a}, df{_dataframe_data = b})
  where
    (a,b) = M.unzip $ _dataframe_data df

transpose :: DataFrame a -> DataFrame a
transpose DataFrame{..} = DataFrame _dataframe_col_names _dataframe_col_names_idx
    _dataframe_row_names _dataframe_row_names_idx $ M.tr _dataframe_data

rowNames :: DataFrame a -> [T.Text]
rowNames DataFrame{..} = V.toList _dataframe_row_names

colNames :: DataFrame a -> [T.Text]
colNames DataFrame{..} = V.toList _dataframe_col_names

mapRows :: (V.Vector a -> V.Vector b) -> DataFrame a -> DataFrame b
mapRows fn df = df
    { _dataframe_data = M.fromRows $ L.map fn $ M.toRows $ _dataframe_data df }

mapCols :: (V.Vector a -> V.Vector b) -> DataFrame a -> DataFrame b
mapCols fn df = df
    { _dataframe_data = M.fromColumns $ L.map fn $ M.toColumns $ _dataframe_data df }

filterRows :: (T.Text -> V.Vector a -> Bool) -> DataFrame a -> DataFrame a
filterRows fn df = df
    { _dataframe_row_names = V.fromList names
    , _dataframe_row_names_idx = HM.fromList $ L.zip names [0..]
    , _dataframe_data = M.fromRows rows }
  where
    (names, rows) = L.unzip $ filter (uncurry fn) $
        L.zip (V.toList $ _dataframe_row_names df) $ M.toRows $ _dataframe_data df

type ReodrderFn a = [(T.Text, V.Vector a)] -> [(T.Text, V.Vector a)]

reorderRows :: ReodrderFn a -> DataFrame a -> DataFrame a
reorderRows fn df
    | isEmpty df = df
    | otherwise = df
        { _dataframe_row_names = V.fromList names
        , _dataframe_row_names_idx = HM.fromList $ L.zip names [0..]
        , _dataframe_data = M.fromRows rows }
  where
    (names, rows) = L.unzip $ fn $ L.zip (V.toList $ _dataframe_row_names df) $
        M.toRows $ _dataframe_data df

reorderColumns :: ReodrderFn a -> DataFrame a -> DataFrame a
reorderColumns fn df
    | isEmpty df = df
    | otherwise = df
        { _dataframe_col_names = V.fromList names
        , _dataframe_col_names_idx = HM.fromList $ L.zip names [0..]
        , _dataframe_data = M.fromColumns cols}
  where
    (names, cols) = L.unzip $ fn $ L.zip (V.toList $ _dataframe_col_names df) $
        M.toColumns $ _dataframe_data df

writeTable :: FilePath -> (a -> T.Text) -> DataFrame a -> IO ()
writeTable output f DataFrame{..} = T.writeFile output $ T.unlines $
    L.map (T.intercalate "\t") $ ("" : V.toList _dataframe_col_names) :
    L.zipWith (:) (V.toList _dataframe_row_names)
    ((L.map . L.map) f $ M.toLists _dataframe_data)

-- | Read data, normalize and calculate p-values.
readData :: FilePath   -- ^ PageRank
         -> FilePath   -- ^ Gene expression
         -> IO (DataFrame (Double, Double))  -- ^ ranks, expression
readData input1 input2 = do
    rank <- readTSV <$> B.readFile input1

    -- Read expression profile and apply "ihs" transformation
    expr <- (fmap ihs' . readTSV) <$> B.readFile input2

    let (labels, xs) = L.unzip $ L.map L.unzip $ groupBy ((==) `on` (fst.fst)) $ sort $
            HM.toList $ HM.intersectionWith (,) rank expr
        rowlab = L.map (T.pack . B.unpack . CI.original) $ fst $ L.unzip $ L.map head labels
        collab = L.map (T.pack . B.unpack . CI.original) $ snd $ L.unzip $ head labels
    return $ mkDataFrame rowlab collab xs

readTSV :: B.ByteString -> HM.HashMap (CI.CI B.ByteString, CI.CI B.ByteString) Double
readTSV input = HM.fromList $ concatMap (f . B.split '\t') content
  where
    f (x:xs) = L.zipWith (\s v -> ((CI.mk x, CI.mk s), readDouble v)) samples xs
    (header:content) = B.lines input
    samples = tail $ B.split '\t' header

readTable :: FilePath -> IO (DataFrame Double)
readTable = readTableWith readDouble

readTableWith :: (B.ByteString -> a) -> FilePath -> IO (DataFrame a)
readTableWith f input = do
    (header:content) <- B.lines <$> B.readFile input
    let samples = tail $ B.split '\t' header
        (rows, dat) = L.unzip $ L.map ((\(x:xs) ->
            (T.pack $ B.unpack x, L.map f xs)) . B.split '\t') content
    return $ mkDataFrame rows (L.map (T.pack . B.unpack) samples) dat

    {-
orderByName :: [T.Text] -> ReodrderFn a
orderByName prefix = sortBy $ \(a,_) (b,_) ->
    let idx1 = findIdx a
        idx2 = findIdx b
    in case () of
        _ | isJust idx1 && isJust idx2 -> case compare idx1 idx2 of
                LT -> LT
                GT -> GT
                EQ -> compare a b
          | otherwise -> compare a b
  where
    findIdx x = go prefix 0
      where
        go (y:ys) !i | isPrefixOf y x = Just i
                     | otherwise = go ys (i+1)
        go _ _ = Nothing
        -}

orderByHClust :: (a -> Double) -> ReodrderFn a 
orderByHClust f xs = flatten $ hclust Ward (V.fromList xs) dist
  where
    dist = euclidean `on` V.map f . snd

orderByKMeans :: Int -> (a -> Double) -> ReodrderFn a 
orderByKMeans k f xs = concat $ fromJust $ clusters $
    kmeansBy k (V.fromList xs) (V.convert . V.map f . snd) defaultKMeansOpts

pearson :: DataFrame Double -> DataFrame Double
pearson DataFrame{..} = DataFrame _dataframe_row_names _dataframe_row_names_idx 
    _dataframe_row_names _dataframe_row_names_idx $ M.fromLists $
    toRowLists $ pearsonMatByRow $ fromRowLists $ M.toLists _dataframe_data 

spearman :: DataFrame Double -> DataFrame Double
spearman DataFrame{..} = DataFrame _dataframe_row_names _dataframe_row_names_idx 
    _dataframe_row_names _dataframe_row_names_idx $ M.fromLists $
    toRowLists $ spearmanMatByRow $ fromRowLists $ M.toLists _dataframe_data 

orderDataFrame :: (a -> Double) -> DataFrame a -> DataFrame a
orderDataFrame f = reorderColumns (orderByHClust f) . reorderRows (orderByHClust f)