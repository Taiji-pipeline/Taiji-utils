{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes #-}

module Taiji.Utils.Plot.ECharts.Scatter
    ( VisualMap(..)
    , scatter3D
    , scatter
    ) where

import Data.Aeson.QQ (aesonQQ)
import Language.Javascript.JMacro
import Data.Aeson
import Data.Maybe
import Control.Arrow
import Data.List (groupBy)
import Data.List.Ordered (nubSort)
import Data.Function (on)
import qualified Data.Text.Lazy as TL
import qualified Text.Blaze.Html5 as H hiding (style)
import qualified Text.Blaze.Html5.Attributes as H
import Text.PrettyPrint.Leijen.Text (renderOneLine, displayT)
import qualified Data.Matrix            as M
import qualified Data.HashMap.Strict as HM

import Taiji.Utils.Plot.ECharts.Types

data VisualMap = Continuous [Double]
               | Categorical [String]

type Point3D = (Double, Double, Double)
type Point2D = (Double, Double)

scatter3D :: [(String, [Point3D])]
          -> VisualMap
          -> EChart
scatter3D dat viz = EChart $ \eid -> displayT $ renderOneLine $ renderJs $
    [jmacro|
        var myChart = echarts.init(document.getElementById(`(eid)`));
        var !dataset = `dataPoints`;
        myChart.setOption(`option`);
    |]
  where
    dataPoints =
        let f (nm, (x, y, z)) v = (nm, [toJSON x, toJSON y, toJSON z, toJSON v])
        in toJSON $ HM.fromList $ map (first head . unzip) $ groupBy ((==) `on` fst) $ case viz of
            Continuous vs -> zipWith f points vs
            Categorical vs ->zipWith f points vs
      where
        points = concatMap (\(x,y) -> zip (repeat x) y) dat
    option = [jmacroE| {
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
        color: [ 
            "#ff7f50", "#87cefa", "#da70d6", "#32cd32", "#6495ed", 
            "#ff69b4", "#ba55d3", "#cd5c5c", "#ffa500", "#40e0d0", 
            "#1e90ff", "#ff6347", "#7b68ee", "#00fa9a", "#ffd700", 
            "#6b8e23", "#ff00ff", "#3cb371", "#b8860b", "#30e0e0" 
        ],
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
        Categorical c -> [jmacroE| {
            type: "piecewise",
            seriesIndex: `[0 .. length dat - 1]`,
            right: 10,
            categories: `nubSort c`,
            inRange: { symbol: { '': 'squre' } }
        } |]

scatter :: [(String, [Point2D])]
        -> VisualMap
        -> EChart
scatter dat viz = EChart $ \eid -> displayT $ renderOneLine $ renderJs $
    [jmacro|
        var myChart = echarts.init(document.getElementById(`(eid)`));
        var !dataset = `dataPoints`;
        myChart.setOption(`option`);
    |]
  where
    dataPoints =
        let f (nm, (x, y)) v = (nm, [toJSON x, toJSON y, toJSON v])
        in toJSON $ HM.fromList $ map (first head . unzip) $ groupBy ((==) `on` fst) $ case viz of
            Continuous vs -> zipWith f points vs
            Categorical vs ->zipWith f points vs
      where
        points = concatMap (\(x,y) -> zip (repeat x) y) dat
    option = [jmacroE| {
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
        color: [ 
            "#ff7f50", "#87cefa", "#da70d6", "#32cd32", "#6495ed", 
            "#ff69b4", "#ba55d3", "#cd5c5c", "#ffa500", "#40e0d0", 
            "#1e90ff", "#ff6347", "#7b68ee", "#00fa9a", "#ffd700", 
            "#6b8e23", "#ff00ff", "#3cb371", "#b8860b", "#30e0e0" 
        ],
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
        Categorical c -> [jmacroE| {
            type: "piecewise",
            seriesIndex: `[0 .. length dat - 1]`,
            right: 10,
            categories: `nubSort c`,
            inRange: { symbol: { '': 'squre' } }
        } |]