{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes #-}

module Taiji.Utils.Plot.ECharts.Line
    ( line
    , line'
    , stackLine
    ) where

import Language.Javascript.JMacro
import qualified Data.Matrix            as M

import Taiji.Utils.Plot.ECharts.Types
import qualified Taiji.Utils.DataFrame as DF

line :: DF.DataFrame Double -> EChart
line df = mkEChart [jmacroE| {
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
        data: `ys`
        } |]

line' :: [(Double, Double)] -> EChart
line' dat = mkEChart [jmacroE| {
    xAxis: {
        type: "value",
        axisLine: {onZero: false}
    },
    tooltip: {
        trigger: "axis"
    }, 
    yAxis: {
        type: "value",
        axisLine: {onZero: false}
    }, 
    series: [{
        data: `dataset`,
        type: "line"
    }]
    } |]
  where
    dataset = map (\(x,y) -> [x,y]) dat

stackLine :: DF.DataFrame Double -> EChart
stackLine df = mkEChart [jmacroE| {
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