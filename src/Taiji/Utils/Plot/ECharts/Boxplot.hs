{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes #-}

module Taiji.Utils.Plot.ECharts.Boxplot (boxplot) where

import Language.Javascript.JMacro
import qualified Data.Matrix            as M
import Statistics.Quantile
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U

import Taiji.Utils.Plot.ECharts.Types
import Taiji.Utils.DataFrame hiding (zip, unzip)

boxplot :: [(T.Text, [Double])] -> EChart
boxplot input = option [jmacroE| {
    grid: { containLabel: true },
    xAxis: {
        type: "category",
        data: `names`,
        axisLabel: { rotate: 45 }
    },
    yAxis: {
        type: "value"
    },
    series: [{
        name: "boxplot",
        type: "boxplot",
        data: `dat'`
    }]
    } |]
  where
    (names, dat) = unzip input
    dat' = flip map dat $ \xs -> quantiles def [0..4] 4 $ U.fromList xs