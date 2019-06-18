{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes #-}

module Taiji.Utils.Plot.ECharts.Bar where

import Data.Aeson.QQ (aesonQQ)
import Language.Javascript.JMacro
import Data.Aeson
import Data.List.Ordered (nubSort)
import Data.Function (on)
import Text.PrettyPrint.Leijen.Text (renderOneLine, displayT)
import qualified Data.Matrix            as M
import qualified Data.HashMap.Strict as HM

import Taiji.Utils.Plot.ECharts.Types
import qualified Taiji.Utils.DataFrame as DF

stackBar :: DF.DataFrame Double -> EChart
stackBar df = EChart $ \eid -> displayT $ renderOneLine $
    renderJs $ [jmacro|
        var myChart = echarts.init(document.getElementById(`(eid)`));
        myChart.setOption(`option`);
        |]
  where
    xlab = DF.colNames df
    ydat = zip (DF.rowNames df) $ M.toLists $ DF._dataframe_data df
    option = [jmacroE| {
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
    dat = flip map ydat $ \(lab, ys) -> [aesonQQ| {
        name: #{lab},
        type: "bar",
        stack: "stack",
        data: #{ys} } |]