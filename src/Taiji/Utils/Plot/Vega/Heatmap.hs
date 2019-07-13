{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes #-}

module Taiji.Utils.Plot.Vega.Heatmap
    ( heatmap
    , contour
    ) where

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

contour :: [(Double, Double)] -> Vega
contour points = option [jmacroE| {
    $schema: "https://vega.github.io/schema/vega/v5.json",
    width: 500,
    height: 400,
    signals: [ {
        name: "count",
        value: 10,
        bind: {input: "select", options: [1, 5, 10, 20]}
    }, {
        name: "points",
        value: true,
        bind: {input: "checkbox"}
    } ],

    data: [ {
        name: "source",
        values: `dat`
    }, {
        name: "contours",
        source: "source",
        transform: [ {
            type: "contour",
            x: {expr: "scale('x', datum.x)"},
            y: {expr: "scale('y', datum.y)"},
            size: [{signal: "width"}, {signal: "height"}],
            count: {signal: "count"}
        } ]
    } ],

    scales: [ {
        name: "x",
        type: "linear",
        round: true,
        nice: true,
        zero: false,
        domain: {data: "source", field: "x"},
        range: "width"
    }, {
        name: "y",
        type: "linear",
        round: true,
        nice: true,
        zero: false,
        domain: {data: "source", field: "y"},
        range: "height"
    }, {
        name: "color",
        type: "linear",
        zero: true,
        domain: {data: "contours", field: "value"},
        range: "heatmap"
    } ],

    axes: [ {
        scale: "x",
        grid: true,
        domain: false,
        orient: "bottom"
    }, {
        scale: "y",
        grid: true,
        domain: false,
        orient: "left"
    } ],

    legends: [{ fill: "color", type: "gradient" }],

    marks: [ {
        type: "path",
        from: {data: "contours"},
        encode: {
            enter: {
                stroke: {value: "#888"},
                strokeWidth: {value: 1},
                fill: {scale: "color", field: "value"},
                fillOpacity: {value: 0.35}
            }
        },
        transform: [ { type: "geopath", field: "datum" } ]
    }, {
        name: "marks",
        type: "symbol",
        from: {data: "source"},
        encode: {
            update: {
                x: {scale: "x", field: "x"},
                y: {scale: "y", field: "y"},
            size: {value: 4},
            fill: [ {test: "points", value: "black"}, {value: "transparent"} ]
            }
        }
    } ],
    config: { range: {heatmap: {scheme: "greenblue"}} }
    } |]
  where
    dat = flip map points $ \(x,y) -> [jmacroE| { x: `x`, y: `y` } |]

