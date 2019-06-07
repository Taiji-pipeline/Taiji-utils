{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE RecordWildCards #-}
module Taiji.Types
    ( TaijiConfig(..)
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
    ) where

import           Bio.Data.Bed
import           Bio.Pipeline.Instances ()
import           Bio.Pipeline.Utils     (Directory)
import Bio.Utils.Misc (readDouble)
import Data.BBI.BigBed (BBedFile)
import           Data.Aeson
import Data.Char
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
    , _taiji_genome       :: Maybe FilePath
    , _taiji_bwa_index    :: Maybe FilePath
    , _taiji_star_index   :: Maybe FilePath
    , _taiji_annotation   :: Maybe FilePath
    , _taiji_rsem_index   :: Maybe FilePath
    , _taiji_genome_index :: Maybe FilePath
    , _taiji_motif_file   :: Maybe FilePath
    , _taiji_tmp_dir      :: Maybe FilePath
    , _taiji_external_network :: Maybe FilePath
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
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = drop 7 }

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
    } deriving (Generic)

instance Binary CellCluster
instance NFData CellCluster

data Cell = Cell
    { _cell_id :: Int
    , _cell_x :: Double
    , _cell_y :: Double
    , _cell_z :: Double
    , _cell_barcode :: B.ByteString
    , _cell_coverage :: Int
    } deriving (Generic)

instance Binary Cell
instance NFData Cell