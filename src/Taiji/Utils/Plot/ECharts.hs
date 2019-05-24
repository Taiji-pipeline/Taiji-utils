{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes #-}

module Taiji.Utils.Plot.ECharts where

import Data.Aeson.QQ (aesonQQ)
import Language.Javascript.JMacro
import Data.Aeson
import Data.Maybe
import qualified Data.Text.Lazy as TL
import qualified Text.Blaze.Html5 as H hiding (style)
import qualified Text.Blaze.Html5.Attributes as H
import Text.PrettyPrint.Leijen.Text (renderOneLine, displayT)
import qualified Data.Matrix            as M

import Taiji.Utils.DataFrame hiding (zip, unzip)

newtype EChart = EChart (String -> TL.Text)

embedEchart :: String -> EChart -> H.Html
embedEchart eid (EChart e) = H.div $ do
    H.div H.! H.id (H.toValue eid) H.! H.style "width: 900px;height:700px;" $ mempty
    H.script H.! H.type_ "text/javascript" $ H.toHtml $ e eid


        {-
stackBar :: T.Text   -- ^ title
         -> [T.Text]  -- ^ X data
         -> [(T.Text, [Double])]  -- ^ Y data
         -> EChart
stackBar title xlab ydat = 
    let dat = flip map ydat $ \(lab, ys) -> [aesonQQ| {
            name: #{lab},
            type: "bar",
            stack: "stack",
            data: #{ys} } |]
    in EChart $ B.unpack $ encode [aesonQQ| {
        toolbox: {
            feature: {
                dataZoom: {},
                restore: {},
                saveAsImage: {}
            }
        },
        grid: {
            left: "3%",
            right: "4%",
            bottom: "3%",
            containLabel: true
        },
        tooltip: {
            trigger: "axis",
            axisPointer : { type : "shadow" }
        }, 
        legend: {
            data: #{fst $ unzip ydat}
        },
        xAxis: [ {
            type : "category",
            data : #{ xlab }
        } ],
        yAxis : [ { type : "value" } ],
        series : #{dat}
        } |]
        -}

punchChart :: [(String, DataFrame (Double, Double))] -> EChart
punchChart dat = EChart $ \eid -> displayT $ renderOneLine $
    renderJs $ [jmacro|
        var myChart = echarts.init(document.getElementById(`(eid)`));
        myChart.setOption(`baseOption`);
        myChart.setOption(`option`);
        |]
  where
    (nms, dfs) = unzip dat
    option = [jmacroE| { options: `map mkPunchChartOpt dfs` } |]
    baseOption = [jmacroE| {
        baseOption: {
            timeline: {
                axisType: 'category',
                data: `map mkTimelineItem nms`
            },
            toolbox: {
                show: true,
                feature: {
                    restore: {},
                    dataZoom: {},
                    saveAsImage: {
                        pixelRatio: 3,
                        excludeComponents: ["dataZoom", "timeline", "toolbox"]
                    }
                }
            },
            series: [{
                name: "Punch Card",
                type: "scatter",
                itemStyle: {
                    emphasis: {
                        borderColor: 'black',
                        borderWidth: 1
                    }
                }
            }],
            visualMap: {
                precision: 2,
                calculable: true,
                bottom: "50%",
                inRange: {
                    color: ["#50a3ba", "#eac736", "#d94e5d"]
                }
            },
            dataZoom: [
                { 
                    type: "slider",
                    show: true,
                    bottom: 60,
                    showDataShadow: false,
                    handleIcon: "M10.7,11.9v-1.3H9.3v1.3c-4.9,0.3-8.8,4.4-8.8,9.4c0,5,3.9,9.1,8.8,9.4v1.3h1.3v-1.3c4.9-0.3,8.8-4.4,8.8-9.4C19.5,16.3,15.6,12.2,10.7,11.9z M13.3,24.4H6.7V23h6.6V24.4z M13.3,19.6H6.7v-1.4h6.6V19.6z",
                    handleSize: "80%",
                    handleStyle: {
                        color: "#fff",
                        shadowBlur: 3,
                        shadowColor: "rgba(0, 0, 0, 0.6)",
                        shadowOffsetX: 2,
                        shadowOffsetY: 2
                    }
                },
                {
                    type: "slider",
                    show: true,
                    yAxisIndex: 0,
                    showDataShadow: false,
                    handleIcon: "M10.7,11.9v-1.3H9.3v1.3c-4.9,0.3-8.8,4.4-8.8,9.4c0,5,3.9,9.1,8.8,9.4v1.3h1.3v-1.3c4.9-0.3,8.8-4.4,8.8-9.4C19.5,16.3,15.6,12.2,10.7,11.9z M13.3,24.4H6.7V23h6.6V24.4z M13.3,19.6H6.7v-1.4h6.6V19.6z",
                    handleSize: "80%",
                    handleStyle: {
                        color: "#fff",
                        shadowBlur: 3,
                        shadowColor: "rgba(0, 0, 0, 0.6)",
                        shadowOffsetX: 2,
                        shadowOffsetY: 2
                    }
                }
            ],
            xAxis: {
                type: "category",
                axisLine: { show: false },
                splitLine: {
                    show: true,
                    lineStyle: {
                        color: "#ededed",
                        type: "dashed"
                    }
                }
            },
            yAxis: {
                type: "category",
                axisLine: { show: false },
                splitLine: {
                    show: true,
                    lineStyle: {
                        color: "#ededed",
                        type: "dashed"
                    }
                }
            },
            legend: {
                data: ["Punch Card"],
                left: "left"
            },
            tooltip: {
                position: "top",
                formatter: function (params) { return(params.value[2]) }
            },
            grid: {
                left: 80,
                top: 50,
                bottom: 100,
                right: 50,
                containLabel: true
            }
        }
    } |]

mkPunchChartOpt :: DataFrame (Double, Double) -> JExpr
mkPunchChartOpt df = option
  where
    df' = reorderColumns (orderByCluster fst) $ reorderRows (orderByCluster fst) df
    dat = zipWith3 (\[j,i] x y -> [i, j, x, y]) idx xs ys
    (ys, xs) = unzip $ concat $ M.toLists $ _dataframe_data df'
    min' = minimum xs
    max' = maximum xs
    nrow = fromIntegral $ M.rows $ _dataframe_data df'
    ncol = fromIntegral $ M.cols $ _dataframe_data df'
    idx = sequence [[nrow - 1, nrow -2 .. 0], [0 .. ncol - 1]]
    option = [jmacroE| {
        series: [{
            data: `dat`,
            symbolSize: function (val) { return
                7 + (val[2] - `min'`) / (`max'` - `min'`) * (25 - 7); }
        }],
        visualMap: {
            min: `if null ys then 0 else minimum ys`,
            max: `if null ys then 0 else maximum ys`
        },
        xAxis: { data: `colNames df'` },
        yAxis: { data: `rowNames df'` }
    } |]

mkTimelineItem :: String -> JExpr
mkTimelineItem s = [jmacroE| {
    value: `s`,
    tooltip: { formatter: "Remove genes with CV less than {b}" },
    symbol: "diamond",
    symbolSize: 16
    } |]

linearMap :: (Double, Double) -> [Double] -> [Double]
linearMap (lo, hi) xs = map f xs
    where
    f x = lo + (x - min') / (max' - min') * (hi - lo)
    min' = minimum xs
    max' = maximum xs
{-# INLINE linearMap #-}