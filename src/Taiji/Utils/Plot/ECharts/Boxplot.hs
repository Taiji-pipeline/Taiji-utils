{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes #-}

module Taiji.Utils.Plot.ECharts.Boxplot (boxplot) where

import Language.Javascript.JMacro
import Statistics.Quantile
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U

import Taiji.Utils.Plot.ECharts.Types

boxplot :: [(T.Text, [Double])] -> EChart
boxplot input = mkEChart [jmacroE| {
    grid: { containLabel: true },
    xAxis: {
        type: "category",
        data: `names`,
        axisLabel: { rotate: 45 }
    },
    yAxis: {
        axisLine: {onZero: false},
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
    dat' = flip map dat $ \xs -> fst $ range $ U.fromList xs

range :: U.Vector Double -> ([Double], U.Vector Double)
range xs = ([U.minimum xs', q1, q2, q3, U.maximum xs'], outliers)
  where
    (xs', outliers) = U.partition (\x -> x >= lower && x <= upper) xs
    lower = q1 - 1.5 * iqr
    upper = q3 + 1.5 * iqr
    iqr = q3 - q1
    [q1, q2, q3] = quantiles def [1, 2, 3] 4 xs