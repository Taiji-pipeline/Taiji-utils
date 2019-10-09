{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes #-}

module Taiji.Utils.Plot.ECharts.Pie (pie) where

import Language.Javascript.JMacro

import Taiji.Utils.Plot.ECharts.Types

pie :: [(String, Double)] -> EChart
pie input = mkEChart [jmacroE| {
    series: [{
        name: "pie",
        type: "pie",
        roseType : 'radius',
        radius: "55%",
        center: ["50%", "60%"],
        data: `dat`
    }]
    } |]
  where
    dat = flip map input $ \(lab, x) -> [jmacroE| {name: `lab`,value: `x`} |]