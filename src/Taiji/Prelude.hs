module Taiji.Prelude
    ( module Bio.Data.Experiment
    , module Bio.Pipeline
    , module Conduit
    , module Control.Monad
    , module Data.Maybe
    , module Data.List
    , module Data.Ord
    , module Taiji.Types

    , module Lens.Micro
    , module Lens.Micro.Mtl
    , (%%~)

    , edge_weight_cutoff
    , on

    , ReaderT
    , asks

    , printf

    , readDouble
    , readInt
    , toShortest
    , packDecimal
    ) where

import           Bio.Data.Experiment
import           Bio.Pipeline
import           Bio.Utils.Misc (readInt, readDouble)
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
import Text.Printf (printf)
import Data.Double.Conversion.ByteString (toShortest)
import Data.ByteString.Lex.Integral (packDecimal)

(%%~) ::  LensLike f s t a b -> (a -> f b) -> s -> f t
(%%~) = id
{-# INLINE (%%~) #-}

infixr 4 %%~

-- | Cutoff for edge weights
edge_weight_cutoff :: Double
edge_weight_cutoff = 0.2