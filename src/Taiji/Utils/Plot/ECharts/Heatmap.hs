{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes #-}

module Taiji.Utils.Plot.ECharts.Heatmap (heatmap) where

import Language.Javascript.JMacro
import qualified Data.Matrix            as M

import Taiji.Utils.Plot.ECharts.Types
import Taiji.Utils.DataFrame hiding (zip, unzip)

heatmap :: DataFrame Double -> EChart
heatmap df = option [jmacroE| {
    grid: { containLabel: true },
    xAxis: {
        type: "category",
        data: `colNames df`,
        axisLabel: { rotate: 45 }
    },
    yAxis: {
        type: "category",
        data: `reverse $ rowNames df`
    },
    visualMap: {
        min: `min'`,
        max: `max'`,
        precision: 2,
        calculable: true,
        realtime: false,
        inRange: {
            color: ["#313695", "#4575b4", "#74add1", "#abd9e9", "#e0f3f8", "#ffffbf", "#fee090", "#fdae61", "#f46d43", "#d73027", "#a50026"]
        }
    },
    series: [{
        name: "Gaussian",
        type: "heatmap",
        data: `dat`,
        itemStyle: {
            emphasis: {
                borderColor: "#333",
                borderWidth: 1
            }
        },
        progressive: 1000,
        animation: false
    }]
    } |]
  where
    dat = zipWith (\[j,i] x -> [i, j, x]) idx values
    values = concat $ M.toLists $ _dataframe_data df
    min' = minimum values
    max' = maximum values
    nrow = fromIntegral $ M.rows $ _dataframe_data df
    ncol = fromIntegral $ M.cols $ _dataframe_data df
    idx = sequence [[nrow - 1, nrow -2 .. 0], [0 .. ncol - 1]]