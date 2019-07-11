{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes #-}

module Taiji.Utils.Plot.Vega.Heatmap (heatmap) where

import Language.Javascript.JMacro
import qualified Data.Matrix            as M

import Taiji.Utils.Plot.Vega.Types
import qualified Taiji.Utils.DataFrame as DF

heatmap :: DF.DataFrame Double -> Vega
heatmap df = option [jmacroE| {
    $schema: "https://vega.github.io/schema/vega-lite/v3.json",
    data: {
        values: `dat`
    },
    mark: "rect",
    width: 300,
    height: 200,
    encoding: {
        x: {
            field: "ix",
            type: "nominal",
            sort: "none"
        },
        y: {
            field: "iy",
            type: "nominal",
            sort: "none"
        },
        color: {
            field: "v",
            type: "quantitative"
        }
    },
    config: {
        view: { stroke: "transparent" }
    }
    } |]
  where
    dat = zipWith f (sequence [DF.rowNames df, DF.colNames df]) $ concat $
        M.toLists $ DF._dataframe_data df
    f [j,i] x = [jmacroE| {
        ix: `i`,
        iy: `j`,
        v: `x`
    } |]

