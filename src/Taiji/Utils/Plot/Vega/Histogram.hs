{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes #-}

module Taiji.Utils.Plot.Vega.Histogram where

import Language.Javascript.JMacro

import Taiji.Utils.Plot.Vega.Types

hist :: [Double] -> Int -> Vega
hist input nbins = option [jmacroE| {
    $schema: "https://vega.github.io/schema/vega-lite/v3.json",
    data: {values: `dat`},
    layer: [ {
        mark: "bar",
        encoding: {
            x: {
                bin: {maxbins: `nbins`},
                field: "x",
                type: "quantitative"
            },
            y: {
                aggregate: "count",
                type: "quantitative"
            }
        }
    } ]
    } |]
  where
    dat = map (\x -> [jmacroE| {x: `x`}|]) input
