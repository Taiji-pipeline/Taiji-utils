{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes #-}

module Taiji.Utils.Plot.ECharts.Graph (graph) where

import Language.Javascript.JMacro

import Taiji.Utils.Plot.ECharts.Types

graph :: (String -> JExpr) -> [String] -> [(String, String)] -> EChart
graph f nodes edges = mkEChart [jmacroE| {
    series: {
        type: "graph",
        layout: "force",
        data: `map f nodes`,
        links: `links`,
        focusNodeAdjacency: true,
        draggable: true,
        roam: true,
        edgeSymbol: ["none", "arrow"],
        edgeSymbolSize: 3,
        force: {
            repulsion: 70
        }
    }
    }|]
  where
    links = flip map edges $ \(fr, to) -> [jmacroE| {source: `fr`, target: `to`} |]