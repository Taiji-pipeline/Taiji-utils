{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes #-}

module Taiji.Utils.Plot.ECharts.Graph (graph) where

import Language.Javascript.JMacro

import Taiji.Utils.Plot.ECharts.Types

graph :: [(String, Double)] -> [(String, String)] -> EChart
graph nodes edges = mkEChart [jmacroE| {
    series: {
        type: "graph",
        layout: "force",
        data: `nodes'`,
        links: `links`,
        focusNodeAdjacency: true
    }
    }|]
  where
    nodes' = flip map (zip names $ linearMap (10, 50) vals) $ \(x, v) -> [jmacroE| {
        name: `x`,
        symbolSize: `v`
    }|]
    links = flip map edges $ \(fr, to) -> [jmacroE| {source: `fr`, target: `to`} |]
    (names, vals) = unzip nodes

linearMap :: (Double, Double) -> [Double] -> [Double]
linearMap (lo, hi) xs = map f xs
    where
    f x = lo + (x - min') / (max' - min') * (hi - lo)
    min' = minimum xs
    max' = maximum xs
{-# INLINE linearMap #-}