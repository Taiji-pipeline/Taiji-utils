{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes #-}

module Taiji.Utils.Plot.Vega.Violin (violin) where

import Language.Javascript.JMacro
import qualified Data.Matrix            as M

import Taiji.Utils.Plot.Vega.Types
import qualified Taiji.Utils.DataFrame as DF

violin :: DF.DataFrame Double -> Vega
  [(T.Text, [Double])] -> VLSpec
violin df = option [jmacroE| {
    $schema: "https://vega.github.io/schema/vega/v5.json",
    width: 500,
    padding: 5,

    config: {
        axisBand: {
            bandPosition: 1,
            tickExtra: true,
            tickOffset: 0
        }
    },

    signals: [
        { name: "fields", value: `fst $ unzip input` },
        { name: "plotWidth", value: 60 },
        { name: "height", update: "(plotWidth + 10) * length(fields)"},
        { name: "trim", value: true, bind: {input: "checkbox"} },
        { name: "bandwidth", value: 0.3, bind: {input: "range", min: 0, max: 1.0, step: 0.01} }
    ],

    data: [ {
        name: "iris",
        values: `input'``
    }, {
        name: "density",
        source: "iris",
        transform: [ {
            type: "kde",
            field: "value",
            groupby: ["key"],
            bandwidth: {signal: "bandwidth"},
            extent: {signal: "trim ? null : [0, 8]"}
        } ]
    }, {
        name: "stats",
        source: "iris",
        transform: [ {
            type: "aggregate",
            groupby: ["key"],
            fields: ["value", "value", "value"],
            ops: ["q1", "median", "q3"],
            as: ["q1", "median", "q3"]
        } ]
    } ],

    scales: [ {
        name: "layout",
        type: "band",
        range: "height",
        domain: {data: "iris", field: "key"}
    }, {
        name: "xscale",
        type: "linear",
        range: "width",
        round: true,
        domain: {data: "iris", field: "value"},
        zero: true,
        nice: true
    }, {
        name: "hscale",
        type: "linear",
        range: [0, {signal: "plotWidth"}],
        domain: {data: "density", field: "density"}
    }, {
        name: "color",
        type: "ordinal",
        domain: {data: "iris", field: "key"},
        range: "category"
    } ],

    axes: [ {
        orient: "bottom",
        scale: "xscale",
        zindex: 1
    }, {
        orient: "left",
        scale: "layout",
        tickCount: 5,
        zindex: 1
    } ],

  "marks": [
    {
      "type": "group",
      "from": {
        "facet": {
          "data": "density",
          "name": "violin",
          "groupby": "key"
        }
      },

      "encode": {
        "enter": {
          "yc": {"scale": "layout", "field": "key", "band": 0.5},
          "height": {"signal": "plotWidth"},
          "width": {"signal": "width"}
        }
      },

      "data": [
        {
          "name": "summary",
          "source": "stats",
          "transform": [
            {
              "type": "filter",
              "expr": "datum.key === parent.key"
            }
          ]
        }
      ],

      "marks": [
        {
          "type": "area",
          "from": {"data": "violin"},
          "encode": {
            "enter": {
              "fill": {"scale": "color", "field": {"parent": "key"}}
            },
            "update": {
              "x": {"scale": "xscale", "field": "value"},
              "yc": {"signal": "plotWidth / 2"},
              "height": {"scale": "hscale", "field": "density"}
            }
          }
        },
        {
          "type": "rect",
          "from": {"data": "summary"},
          "encode": {
            "enter": {
              "fill": {"value": "black"},
              "height": {"value": 2}
            },
            "update": {
              "x": {"scale": "xscale", "field": "q1"},
              "x2": {"scale": "xscale", "field": "q3"},
              "yc": {"signal": "plotWidth / 2"}
            }
          }
        },
        {
          "type": "rect",
          "from": {"data": "summary"},
          "encode": {
            "enter": {
              "fill": {"value": "black"},
              "width": {"value": 2},
              "height": {"value": 8}
            },
            "update": {
              "x": {"scale": "xscale", "field": "median"},
              "yc": {"signal": "plotWidth / 2"}
            }
          }
        }
      ]
    }
  ]
 } |]
    let input' = flip concatMap input $ \(x,ys) -> flip map ys $ \y ->
            M.fromList [("key"::T.Text, toJSON x), ("value", toJSON y)]