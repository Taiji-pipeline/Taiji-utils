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
    ) where

import           Bio.Data.Bed
import Control.Lens
import           Bio.Pipeline.Instances ()
import           Bio.Pipeline.Utils     (Directory)
import Bio.Utils.Misc (readDouble)
import           Data.Aeson
import Data.Char
import qualified Data.ByteString.Char8  as B
import           Data.CaseInsensitive   (CI, mk, original)
import           Data.Default.Class
import Data.Maybe
import           Data.Serialize         (Serialize (..))
import           GHC.Generics           (Generic)
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

instance Default TaijiConfig where
    def = TaijiConfig
        { _taiji_output_dir = "output"
        , _taiji_input = "input.yml"
        , _taiji_genome = def
        , _taiji_bwa_index = def
        , _taiji_star_index = def
        , _taiji_annotation = def
        , _taiji_rsem_index = def
        , _taiji_genome_index = def
        , _taiji_motif_file = def
        , _taiji_tmp_dir = def
        , _taiji_external_network = def
        }

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

instance Serialize NetNode

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

instance Serialize Value where
    put x = put $ encode x
    get = fromJust . decode <$> get
