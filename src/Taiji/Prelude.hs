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
    , (%%~)
    , (.=)
    , mapped
    , traversed
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

(%%~) ::  LensLike f s t a b -> (a -> f b) -> s -> f t
(%%~) = id
{-# INLINE (%%~) #-}

infixr 4 %%~

-- | Cutoff for edge weights
edge_weight_cutoff :: Double
edge_weight_cutoff = 0.2