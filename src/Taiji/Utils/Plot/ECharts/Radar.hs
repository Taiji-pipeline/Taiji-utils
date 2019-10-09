{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes #-}

module Taiji.Utils.Plot.ECharts.Radar (radar) where

import Language.Javascript.JMacro
import qualified Data.Matrix            as M
import qualified Data.Vector as V

import Taiji.Utils.Plot.ECharts.Types
import qualified Taiji.Utils.DataFrame as DF

radar :: DF.DataFrame Double -> EChart
radar df = mkEChart [jmacroE| {
    grid: { containLabel: true },
    legend: {
        data: `fst $ unzip ydat`
    },
    radar: {
        shape: "circle",
        name: {
            textStyle: {
                color: "#fff",
                backgroundColor: "#999",
                borderRadius: 5,
                padding: [3, 5]
           }
        },
        indicator: `indicator`
    },
    series: {
        type: "radar",
        data: `dat`
    }
    } |]
  where
    indicator = let max' = map V.maximum $ M.toColumns $ DF._dataframe_data df
                in zipWith (\m x -> [jmacroE| {name: `x`, max: `m`} |]) max' $ DF.colNames df
    ydat = zip (DF.rowNames df) $ M.toLists $ DF._dataframe_data df
    dat = flip map ydat $ \(lab, ys) -> [jmacroE| { name: `lab`, value: `ys`} |]