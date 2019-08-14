{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes #-}

module Taiji.Utils.Plot.ECharts.Bar where

import Language.Javascript.JMacro
import qualified Data.Matrix            as M

import Taiji.Utils.Plot.ECharts.Types
import qualified Taiji.Utils.DataFrame as DF

stackBar :: DF.DataFrame Double -> EChart
stackBar df = option [jmacroE| {
    grid: { containLabel: true },
    tooltip: {
        trigger: "axis",
        axisPointer : { type : "shadow" }
    }, 
    legend: {
        data: `fst $ unzip ydat`
    },
    xAxis: {
        type : "category",
        data : `xlab`,
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
        type: "bar",
        stack: "stack",
        data: `ys`
        } |]