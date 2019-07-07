{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes #-}

module Taiji.Utils.Plot.ECharts.Scatter
    ( VisualMap(..)
    , scatter3D
    , scatter
    ) where

import Language.Javascript.JMacro
import Data.Aeson
import Control.Arrow
import Data.List (groupBy)
import Data.List.Ordered (nubSort)
import Data.Function (on)
import qualified Data.HashMap.Strict as HM

import Taiji.Utils.Plot.ECharts.Types

data VisualMap = Continuous [Double]
               | Categorical [String]

type Point3D = (Double, Double, Double)
type Point2D = (Double, Double)

scatter3D :: [(String, [Point3D])]
          -> VisualMap
          -> EChart
scatter3D dat viz = o <> js [jmacro| var !dataset = `dataPoints`; |]
  where
    dataPoints =
        let f (nm, (x, y, z)) v = (nm, [toJSON x, toJSON y, toJSON z, toJSON v])
        in toJSON $ HM.fromList $ map (first head . unzip) $ groupBy ((==) `on` fst) $ case viz of
            Continuous vs -> zipWith f points vs
            Categorical vs ->zipWith f points vs
      where
        points = concatMap (\(x,y) -> zip (repeat x) y) dat
    o = option [jmacroE| {
        grid3D: [ {
            width : "45%",
            axisTick:false,
            axisLabel:false,
            axisPointer:false
        }, {
            width : "45%",
            left: "50%",
            axisTick:false,
            axisLabel:false,
            axisPointer:false
        } ],
        xAxis3D: [ {
            grid3DIndex: 0,
            name: "dim1"
        }, {
            grid3DIndex: 1,
            name: "dim1"
        } ],
        yAxis3D: [ {
            grid3DIndex: 0,
            name: "dim2"
        }, {
            grid3DIndex: 1,
            name: "dim2"
        } ],
        zAxis3D: [ {
            grid3DIndex: 0,
            name: "dim3"
        }, {
            grid3DIndex: 1,
            name: "dim3"
        } ],
        series: `map (mkSeries 1) dat ++ map (mkSeries 0) dat`,
        legend: { data: `map fst dat` },
        visualMap: `visualMap`,
        color: `defColors`,
        toolbox: {
            show: true,
            feature: {
                saveAsImage: {
                    pixelRatio: 3,
                    excludeComponents: ["toolbox"]
                }
            }
        }
    } |]
    mkSeries i (label, _) = [jmacroE| {
        grid3DIndex: `i::Int`,
        type: 'scatter3D',
        symbolSize: 2.3,
        name: `label`,
        data: dataset[`label`]
    } |]
    visualMap = case viz of
        Continuous vs -> [jmacroE| {
            seriesIndex: `[0 .. length dat - 1]`,
            precision: 2,
            calculable: true,
            min: `minimum vs`,
            max: `maximum vs`,
            right: 10,
            inRange: {
                color: ["#50a3ba", "#eac736", "#d94e5d"]
            }
        } |]
        Categorical c -> 
            let cats = nubSort c
                colors = take (length cats) $ cycle defColors
            in [jmacroE| {
                type: "piecewise",
                seriesIndex: `[0 .. length dat - 1]`,
                right: 10,
                categories: `cats`,
                inRange: { color: `colors`}
               } |]

scatter :: [(String, [Point2D])]
        -> VisualMap
        -> EChart
scatter dat viz = o <> js [jmacro| var !dataset = `dataPoints`; |]
  where
    dataPoints =
        let f (nm, (x, y)) v = (nm, [toJSON x, toJSON y, toJSON v])
        in toJSON $ HM.fromList $ map (first head . unzip) $ groupBy ((==) `on` fst) $ case viz of
            Continuous vs -> zipWith f points vs
            Categorical vs ->zipWith f points vs
      where
        points = concatMap (\(x,y) -> zip (repeat x) y) dat
    o = option [jmacroE| {
        grid: [ {
            width : "40%",
            show:true
        }, {
            width : "40%",
            left: "50%",
            show:true
        } ],
        xAxis: [ {
            gridIndex: 0,
            axisTick: {show:false},
            axisLabel: {show:false},
            splitLine: {show:false},
            axisLine: {onZero: false},
            name: "dim1"
        }, {
            gridIndex: 1,
            axisTick: {show:false},
            axisLabel: {show:false},
            splitLine: {show:false},
            axisLine: {onZero: false},
            name: "dim1"
        } ],
        yAxis: [ {
            gridIndex: 0,
            axisTick: {show:false},
            axisLabel: {show:false},
            splitLine: {show:false},
            axisLine: {onZero: false},
            name: "dim2"
        }, {
            gridIndex: 1,
            axisTick: {show:false},
            axisLabel: {show:false},
            splitLine: {show:false},
            axisLine: {onZero: false},
            name: "dim2"
        } ],
        series: `map (mkSeries 1) dat ++ map (mkSeries 0) dat`,
        legend: { data: `map fst dat` },
        visualMap: `visualMap`,
        color: `defColors`,
        toolbox: {
            show: true,
            feature: {
                saveAsImage: {
                    pixelRatio: 3,
                    excludeComponents: ["toolbox"]
                }
            }
        }
    } |]
    mkSeries i (label, _) = [jmacroE| {
        xAxisIndex: `i::Int`,
        yAxisIndex: `i::Int`,
        type: 'scatter',
        symbolSize: 1.5,
        name: `label`,
        data: dataset[`label`]
    } |]
    visualMap = case viz of
        Continuous vs -> [jmacroE| {
            seriesIndex: `[0 .. length dat - 1]`,
            precision: 2,
            calculable: true,
            min: `minimum vs`,
            max: `maximum vs`,
            right: 10,
            inRange: {
                color: ["#50a3ba", "#eac736", "#d94e5d"]
            }
        } |]
        Categorical c -> 
            let cats = nubSort c
                colors = take (length cats) $ cycle defColors
            in [jmacroE| {
                type: "piecewise",
                seriesIndex: `[0 .. length dat - 1]`,
                right: 10,
                categories: `cats`,
                inRange: { color: `colors`}
               } |]

