{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes #-}

module Taiji.Utils.Plot.ECharts.Scatter
    ( VisualMap(..)
    , scatter3D
    , scatter3D'
    , scatter
    ) where

import Language.Javascript.JMacro
import Data.Aeson
import Control.Arrow
import Data.List (groupBy, transpose)
import Data.List.Ordered (nubSort)
import Data.Function (on)
import qualified Data.HashMap.Strict as HM
import Text.Printf (printf)

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
        color: `defColors`
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
            height: 500,
            width: 500,
            left: 50,
            containLabel: true,
            show:true
        }, {
            height: 500,
            width: 500,
            right: 50,
            containLabel: true,
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
        legend: { data: `map fst dat`, orient: 'vertical', top: 50 },
        visualMap: `visualMap`,
        color: `defColors`
    } |]
    mkSeries i (label, _) = [jmacroE| {
        xAxisIndex: `i::Int`,
        yAxisIndex: `i::Int`,
        type: 'scatterGL',
        symbolSize: 1.8,
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

scatter3D' :: [(String, [Point3D])]
          -> [(String, [Double])]   -- a list of color map
          -> EChart
scatter3D' dat viz = o <> jsCode
  where
    select :: String
    select = concat $
        zipWith (\idx (nm,_) -> printf "<option value=%d>%s</option>" idx nm)
        [3::Int ..] viz
    jsCode = js [jmacro|
        config.innerHTML = "<select onChange='mkChange(this)'>" + `select`;

        var chart = myChart;

        function !mkChange(sel) {
            var x = sel.value;
            var total = Object.entries(dataset).reduce(function (total, pair) {
                var col = pair[1].map(function(val,idx) { return val[x]; });
                return total.concat(col);
            }, []);

            chart.setOption({
                visualMap: {
                    max: Math.max.apply(null, total),
                    min: Math.min.apply(null, total),
                    dimension: x
                }
            });
        }
        var !dataset = `dataPoints`;
    |]
    dataPoints = toJSON $ HM.fromList $ map (first head . unzip) $ groupBy ((==) `on` fst) $
        zipWith f points cols
      where
        f (nm, (x, y, z)) v = (nm, [toJSON x, toJSON y, toJSON z] ++ map toJSON v)
        points = concatMap (\(x,y) -> zip (repeat x) y) dat
        cols = transpose $ map snd viz
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
        color: `defColors`
    } |]
    mkSeries i (label, _) = [jmacroE| {
        grid3DIndex: `i::Int`,
        type: 'scatter3D',
        symbolSize: 2.3,
        name: `label`,
        data: dataset[`label`]
    } |]
    visualMap = [jmacroE| {
            seriesIndex: `[0 .. length dat - 1]`,
            precision: 2,
            calculable: true,
            right: 10,
            min: `minimum $ snd $ head viz`,
            max: `maximum $ snd $ head viz`,
            inRange: {
                color: ["#50a3ba", "#eac736", "#d94e5d"]
            }
    } |]