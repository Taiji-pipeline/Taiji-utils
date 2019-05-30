module Taiji.Prelude
    ( module Bio.Data.Experiment
    , module Conduit
    , module Control.Monad
    , module Data.Maybe
    , module Data.List
    , module Data.Ord
    , module Taiji.Types
    , edge_weight_cutoff
    , on
    , (^.)
    , (.~)
    , (&)
    , (%~)
    , (.=)
    , mapped
    , traversed
    , traverseOf
    , traverseOf_
    , _1
    , _2
    , _3
    , _Just

    , ReaderT
    , asks
    ) where

import           Bio.Data.Experiment
import           Conduit
import Control.Monad
import Lens.Micro
import Lens.Micro.Mtl
import Data.List
import           Data.Maybe
import Data.Function (on)
import Data.Ord
import           Taiji.Types
import           Control.Monad.Reader              (asks, ReaderT, liftIO)

-- | Cutoff for edge weights
edge_weight_cutoff :: Double
edge_weight_cutoff = 0.2