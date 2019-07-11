{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes #-}

module Taiji.Utils.Plot.ECharts.Line (stackLine) where

import Language.Javascript.JMacro
import qualified Data.Matrix            as M

import Taiji.Utils.Plot.ECharts.Types
import qualified Taiji.Utils.DataFrame as DF

stackLine :: DF.DataFrame Double -> EChart
stackLine df = option [jmacroE| {
    grid: {
        left: "3%",
        right: "4%",
        bottom: "3%",
        containLabel: true
    },
    tooltip: {
        trigger: "axis"
    }, 
    legend: {
        data: `fst $ unzip ydat`
    },
    xAxis: {
        type : "category",
        data : `xlab`,
        boundaryGap: false,
        axisLabel: { rotate: 45 }
    },
    yAxis : { type : "value" },
    series : `dat`
    } |]
  where
    xlab = DF.colNames df
    ydat = zip (DF.rowNames df) $ M.toLists $ DF._dataframe_data df
    dat = flip map ydat $ \(lab, ys) -> [jmacroE| {
        name: `lab`,
        type: "line",
        stack: "stack",
        data: `ys`
        } |]