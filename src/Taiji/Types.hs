{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards #-}
module Taiji.Types
    ( TaijiConfig(..)
    , fetchGenome
    , fetchAnnotation
    , fetchMotif

    , GeneName
    , Promoter
    , RegDomain
    , DomainType(..)
    , NetNode(..)
    , nodeToLine
    , nodeFromLine
    , EdgeType(..)
    , NetEdge(..)
    , edgeToLine

    , SiteAffinity
    , toSiteAffinity
    , getSiteAffinity
    , PeakAffinity
    , toPeakAffinity
    , getPeakAffinity

    , BBIndex
    , TFBS
    , SiteInfo(..)

    , CellCluster(..)
    , Cell(..)

    , Optimizer(..)
    ) where

import           Bio.Data.Bed
import           Bio.Pipeline.Instances ()
import           Bio.Pipeline.Utils     (Directory)
import Bio.Pipeline.Download (getUrl)
import Bio.Utils.Misc (readDouble)
import Data.BBI.BigBed (BBedFile)
import           Data.Aeson
import Data.Char
import qualified Data.Text as T
import Lens.Micro ((^.))
import qualified Data.ByteString.Char8  as B
import qualified Data.HashMap.Strict as M
import           Data.CaseInsensitive   (CI, mk, original)
import           Data.Default.Class
import Data.Maybe
import           Data.Binary (Binary(..))
import qualified Data.Serialize as S
import           GHC.Generics           (Generic)
import Control.DeepSeq (NFData)
import Data.Double.Conversion.ByteString (toShortest)

data TaijiConfig = TaijiConfig
    { _taiji_output_dir   :: Directory
    , _taiji_input        :: FilePath
    , _taiji_assembly     :: Maybe String
    , _taiji_genome       :: Maybe FilePath
    , _taiji_annotation   :: Maybe FilePath
    , _taiji_motif_file   :: Maybe FilePath
    , _taiji_tmp_dir      :: Maybe FilePath
    , _taiji_external_network :: Maybe FilePath
    , _taiji_scatac_cluster_resolution_list :: [Double]
    , _taiji_scatac_cluster_resolution :: Maybe Double
    , _taiji_cluster_optimizer :: Optimizer
    , _taiji_blacklist :: Maybe FilePath
    , _taiji_callpeak_fdr :: Maybe Double
    , _taiji_callpeak_genome_size :: Maybe String
    , _taiji_scatac_cell_barcode_length :: Maybe Int
    , _taiji_scatac_te_cutoff :: Maybe Double
    , _taiji_scatac_minimal_fragment :: Int
    , _taiji_scrna_cell_barcode_length :: Maybe Int
    , _taiji_scrna_umi_length :: Maybe Int
    , _taiji_scrna_doublet_score_cutoff :: Double
    , _taiji_scrna_cluster_resolution_list :: [Double]
    , _taiji_scrna_cluster_resolution :: Maybe Double
    , _taiji_bwa_index    :: FilePath
    , _taiji_bwa_seed_length :: Int
    , _taiji_star_index   :: FilePath
    , _taiji_genome_index :: FilePath
    , _taiji_rsem_index   :: FilePath
    } deriving (Generic)

instance Binary TaijiConfig

-- Drop "_taiji_" prefix
instance ToJSON TaijiConfig where
    toJSON = genericToJSON defaultOptions
        { fieldLabelModifier = drop 7 }
    toEncoding = genericToEncoding defaultOptions
        { fieldLabelModifier = drop 7 }

-- Drop "_taiji_" prefix
instance FromJSON TaijiConfig where
    parseJSON = withObject "TaijiConfig" $ \v -> do
        let String dir = M.lookupDefault (error "no output_dir") "output_dir" v
            genomeDir = T.unpack dir ++ "/GENOME/"
            assembly = case M.lookup "assembly" v of
                Just (String x) -> Just $ T.unpack $ T.toUpper x
                _ -> Nothing
            resolutions = [0.005, 0.01, 0.02, 0.04, 0.08, 0.16, 0.32, 0.64, 0.8, 1]
        TaijiConfig
            <$> v .:? "output_dir" .!= "output/"
            <*> v .: "input"
            <*> return assembly
            <*> v .:? "genome" .!= fmap (\x -> genomeDir ++ x ++ ".fasta") assembly
            <*> v .:? "annotation" .!= fmap (\x -> genomeDir ++ x ++ ".gtf") assembly
            <*> v .:? "motif_file" .!= fmap (\x -> genomeDir ++ x ++ ".meme") assembly
            <*> v .:? "tmp_dir"
            <*> v .:? "external_network"
            <*> v .:? "scatac_cluster_resolution_list" .!= resolutions
            <*> v .:? "scatac_cluster_resolution"
            <*> v .:? "cluster_optimizer" .!= RBConfiguration
            <*> v .:? "blacklist"
            <*> v .:? "callpeak_fdr"
            <*> v .:? "callpeak_genome_size" .!= ( case assembly of
                Nothing -> Nothing
                Just ass -> case () of
                    _ | ass `elem` mouseGenome -> Just "mm"
                      | ass `elem` humanGenome -> Just "hs"
                      | otherwise -> Nothing )
            <*> v .:? "scatac_cell_barcode_length"
            <*> v .:? "tss_enrichment_cutoff"
            <*> v .:? "scatac_fragment_cutoff" .!= 1000
            <*> v .:? "scrna_cell_barcode_length"
            <*> v .:? "scrna_umi_length"
            <*> v .:? "scrna_doublet_score_cutoff" .!= 0.5
            <*> v .:? "scrna_cluster_resolution_list" .!= resolutions
            <*> v .:? "scrna_cluster_resolution"
            <*> v .:? "bwa_index" .!=
                (genomeDir <> "BWA_index/" <> fromMaybe "genome" assembly <> ".fa")
            <*> v .:? "bwa_seed_length" .!= 32
            <*> v .:? "star_index" .!= (genomeDir ++ "STAR_index/")
            <*> v .:? "genome_index" .!=
                (genomeDir <> fromMaybe "genome" assembly <> ".index")
            <*> v .:? "rsem_index" .!=
                (genomeDir <> "RSEM_index/" <> fromMaybe "genome" assembly)

mouseGenome :: [String]
mouseGenome = ["GRCM38", "MM10"]

humanGenome :: [String]
humanGenome = ["GRCH38", "HG38"]

fetchGenome :: FilePath -> String -> IO ()
fetchGenome output assembly 
    | assembly == "GRCH38" || assembly == "HG38" = getUrl output 
        "ftp://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_human/release_31/GRCh38.primary_assembly.genome.fa.gz" True
    | assembly == "GRCM38" || assembly == "MM10" = getUrl output 
        "ftp://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_mouse/release_M22/GRCm38.primary_assembly.genome.fa.gz" True
    | assembly == "MM_TEST" = getUrl output
        "ftp://hgdownload.soe.ucsc.edu/goldenPath/mm10/chromosomes/chr19.fa.gz" True
    | otherwise = error "Unknown assembly"

fetchAnnotation :: FilePath -> String -> IO ()
fetchAnnotation output assembly
    | assembly == "GRCH38" || assembly == "HG38" = getUrl output 
        "ftp://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_human/release_33/gencode.v33.basic.annotation.gtf.gz" True
    | assembly `elem` ["GRCM38", "MM10", "MM_TEST"] = getUrl output 
        "ftp://ftp.ebi.ac.uk/pub/databases/gencode/Gencode_mouse/release_M24/gencode.vM24.basic.annotation.gtf.gz" True
    | otherwise = error "Unknown assembly"

fetchMotif :: FilePath -> String -> IO ()
fetchMotif output assembly
    | assembly == "GRCH38" || assembly == "HG38" = getUrl output 
        "https://taiji-pipeline.github.io/documentation/_downloads/cisBP_human.meme" False
    | assembly `elem` ["GRCM38", "MM10", "MM_TEST"] = getUrl output 
        "https://taiji-pipeline.github.io/documentation/_downloads/cisBP_mouse.meme" False
    | otherwise = error "Unknown assembly"

type GeneName = CI B.ByteString
type Promoter = BEDExt BED3 GeneName
type RegDomain = BEDExt BED3 GeneName

data DomainType = Promoter
                | Enhancer
                deriving (Eq)

data NetNode = NetNode
    { _node_name              :: GeneName
    , _node_weight            :: Double
    , _node_expression        :: Maybe Double
    } deriving (Generic, Show, Read, Eq, Ord)

instance S.Serialize (CI B.ByteString) where
    put = S.put . original
    get = fmap mk S.get

instance S.Serialize NetNode

nodeToLine :: NetNode -> B.ByteString
nodeToLine NetNode{..} = B.intercalate ","
    [ B.map toUpper $ original _node_name
    , toShortest _node_weight
    , fromMaybe "" $ fmap toShortest _node_expression
    ]

nodeFromLine :: B.ByteString -> NetNode
nodeFromLine l = NetNode (mk f1) (readDouble f2)
    (if B.null f3 then Nothing else Just $ readDouble f3)
  where
    [f1,f2,f3] = B.split ',' l

data EdgeType =
      Binding { _edge_binding_locus :: BED3
              , _edge_binding_annotation :: B.ByteString
              , _edge_binding_affinity :: Double }
    | Combined Double
    deriving (Generic, Show, Read)

data NetEdge = NetEdge
    { _edge_from :: CI B.ByteString
    , _edge_to :: CI B.ByteString
    , _edge_type :: EdgeType
    } deriving (Generic, Show, Read)

edgeToLine :: NetEdge -> B.ByteString
edgeToLine NetEdge{..} = B.intercalate "," $
    [ B.map toUpper $ original _edge_from
    , B.map toUpper $ original _edge_to
    ] ++ f _edge_type
  where
    f Binding{..} = [ _edge_binding_locus^.chrom
                    , B.pack $ show $ _edge_binding_locus^.chromStart 
                    , B.pack $ show $ _edge_binding_locus^.chromEnd
                    , _edge_binding_annotation
                    , toShortest _edge_binding_affinity
                    , "BIND"]
    f (Combined w) = [toShortest w, "COMBINED_REGULATE"]

instance Default (CI B.ByteString) where
    def = ""

instance Binary Value where
    put x = put $ encode x
    get = fromJust . decode <$> get

-- | Affinity score of a TF binding site, from 0 to 1.
newtype SiteAffinity = SiteAffinity
    { getSiteAffinity :: Double } deriving (Ord, Eq)

-- | Convert score [0,1000] to affinity score [0,1].
toSiteAffinity :: Int -> SiteAffinity
toSiteAffinity x = SiteAffinity $ fromIntegral x / 1000
{-# INLINE toSiteAffinity #-}

-- | Affinity score of a peak, from 0 to 1.
newtype PeakAffinity = PeakAffinity
    { getPeakAffinity :: Double } deriving (Ord, Eq)

-- | Convert p-value to affinity score [0,1].
toPeakAffinity :: Double -> PeakAffinity
toPeakAffinity x = PeakAffinity $ 1 / (1 + exp (-(x - 5)))
{-# INLINE toPeakAffinity #-}

type BBIndex = M.HashMap B.ByteString BBedFile

type TFBS = BEDExt BED3 SiteInfo

data SiteInfo = SiteInfo
    { _tf_name :: CI B.ByteString
    , _site_affinity :: SiteAffinity
    , _peak_affinity :: PeakAffinity }


data CellCluster = CellCluster
    { _cluster_name :: B.ByteString
    , _cluster_member :: [Cell]
    } deriving (Generic, Show)

instance Binary CellCluster
instance NFData CellCluster

data Cell = Cell
    { _cell_id :: Int
    , _cell_2d :: (Double, Double)
    , _cell_barcode :: B.ByteString
    , _cell_coverage :: Int
    } deriving (Generic, Show)

instance Binary Cell
instance NFData Cell

data Optimizer = RBConfiguration
               | CPM
               deriving (Show, Generic)
instance Binary Optimizer
instance FromJSON Optimizer
instance ToJSON Optimizer