{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes #-}

module Taiji.Utils.Plot.ECharts where

import Data.Aeson.QQ (aesonQQ)
import Language.Javascript.JMacro
import Data.Aeson
import Data.Maybe
import qualified Data.Text as T
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

punchChart :: DataFrame (Double, Double) -> EChart
punchChart df = EChart $ \eid -> displayT $ renderOneLine $
    renderJs $ [jmacro|
        var myChart = echarts.init(document.getElementById(`(eid)`));
        myChart.setOption(`option`);
        |]
  where
    df' = reorderColumns (orderByCluster fst) $ reorderRows (orderByCluster fst) df
    dat = zipWith3 (\[j,i] x y -> [i, j, x, y]) idx (linearMap (7, 25) xs) ys
    (xs, ys) = unzip $ concat $ M.toLists $ _dataframe_data df'
    nrow = fromIntegral $ M.rows $ _dataframe_data df'
    ncol = fromIntegral $ M.cols $ _dataframe_data df'
    idx = sequence [[nrow - 1, nrow -2 .. 0], [0 .. ncol - 1]]
    option = [jmacroE| {
        series: [{
            data: `dat`,
            name: "Punch Card",
            type: "scatter",
            symbolSize: function (val) { return val[2]; },
            itemStyle: {
                emphasis: {
                    borderColor: 'black',
                    borderWidth: 1
                }
            }
        }],
        visualMap: {
            min: `minimum ys`,
            max: `maximum ys`,
            precision: 2,
            calculable: true,
            inRange: {
                color: ["#50a3ba", "#eac736", "#d94e5d"]
            }
        },
        dataZoom: [
            { show: true },
            {
                show: true,
                yAxisIndex: 0
            }
        ],
        xAxis: {
            type: "category",
            data: `colNames df'`,
            axisLine: { show: false },
            splitLine: {
                show: true,
                lineStyle: {
                    color: "#999",
                    type: "dashed"
                }
            }
        },
        yAxis: {
            type: "category",
            data: `rowNames df'`,
            axisLine: { show: false },
            splitLine: {
                show: true,
                lineStyle: {
                    color: "#999",
                    type: "dashed"
                }
            }
        },
        legend: {
            data: ["Punch Card"],
            left: "right"
        },
        tooltip: {
            position: "top"
        },
        grid: {
            left: 80,
            top: 30,
            bottom: 10,
            right: 50,
            containLabel: true
        }
    } |]


linearMap :: (Double, Double) -> [Double] -> [Double]
linearMap (lo, hi) xs = map f xs
    where
    f x = lo + (x - min') / (max' - min') * (hi - lo)
    min' = minimum xs
    max' = maximum xs
{-# INLINE linearMap #-}