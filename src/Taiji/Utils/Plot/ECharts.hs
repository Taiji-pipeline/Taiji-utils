{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes #-}

module Taiji.Utils.Plot.ECharts where

import Data.Aeson.QQ (aesonQQ)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H hiding (style)
import Text.Printf (printf)
import qualified Text.Blaze.Html5.Attributes as H

newtype EChart = EChart { fromEChart :: Value }

embedEchart :: String -> EChart -> H.Html
embedEchart eid (EChart e) = H.div $ do
    H.div H.! H.id (H.toValue eid) H.! H.style "width: 600px;height:400px;" $ mempty
    H.script H.! H.type_ "text/javascript" $ H.toHtml (content :: String)
  where
    content = printf (
        "var myChart = echarts.init(document.getElementById('%s'));" ++
        "var option = %s;myChart.setOption(option);" ) eid (B.unpack $ encode e)

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
    in EChart [aesonQQ| {
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