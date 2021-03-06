{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes #-}

module Taiji.Utils.Plot.Vega.Violin (violin, defaultAxis, Axis(..)) where

import Language.Javascript.JMacro
import qualified Data.Text as T

import Taiji.Utils.Plot.Vega.Types

data Axis = Axis
    { _axis_type :: String
    , _axis_title :: String
    }

defaultAxis :: Axis
defaultAxis = Axis
    { _axis_type = "linear"
    , _axis_title = ""
    }

violin :: [(T.Text, [Double])] -> Axis -> Vega
violin input axis = option [jmacroE| {
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
        { name: "height", update: "(plotWidth + 10) * length(fields)"}
    ],

    data: [ {
        name: "violin",
        values: `input'`
    }, {
        name: "density",
        source: "violin",
        transform: [ {
            type: "kde",
            field: "value",
            groupby: ["key"]
        } ]
    }, {
        name: "stats",
        source: "violin",
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
        domain: {data: "violin", field: "key"}
    }, {
        name: "xscale",
        type: `_axis_type axis`,
        range: "width",
        round: true,
        domain: {data: "violin", field: "value"},
        zero: false,
        nice: true
    }, {
        name: "hscale",
        type: "linear",
        range: [0, {signal: "plotWidth"}],
        domain: {data: "density", field: "density"}
    }, {
        name: "color",
        type: "ordinal",
        domain: {data: "violin", field: "key"},
        range: "category"
    } ],

    axes: [ {
        orient: "bottom",
        scale: "xscale",
        title: `_axis_title axis`,
        zindex: 1
    }, {
        orient: "left",
        scale: "layout",
        tickCount: 5,
        zindex: 1
    } ],

  marks: [
    {
      type: "group",
      from: {
        facet: {
          data: "density",
          name: "violin",
          groupby: "key"
        }
      },

      encode: {
        enter: {
          yc: {scale: "layout", field: "key", band: 0.5},
          height: {signal: "plotWidth"},
          width: {signal: "width"}
        }
      },

      data: [
        {
          name: "summary",
          source: "stats",
          transform: [
            {
              type: "filter",
              expr: "datum.key === parent.key"
            }
          ]
        }
      ],

      marks: [
        {
          type: "area",
          from: {data: "violin"},
          encode: {
            enter: {
              fill: {scale: "color", field: {parent: "key"}}
            },
            update: {
              x: {scale: "xscale", field: "value"},
              yc: {signal: "plotWidth / 2"},
              height: {scale: "hscale", field: "density"}
            }
          }
        },
        {
          type: "rect",
          from: {data: "summary"},
          encode: {
            enter: {
              fill: {value: "black"},
              height: {value: 2}
            },
            update: {
              x: {scale: "xscale", field: "q1"},
              x2: {scale: "xscale", field: "q3"},
              yc: {signal: "plotWidth / 2"}
            }
          }
        },
        {
          type: "rect",
          from: {data: "summary"},
          encode: {
            enter: {
              fill: {value: "black"},
              width: {value: 2},
              height: {value: 8}
            },
            update: {
              x: {scale: "xscale", field: "median"},
              yc: {signal: "plotWidth / 2"}
            }
          }
        }
      ]
    }
  ]
 } |]
  where
    input' = flip concatMap input $ \(nm,vals) -> flip map vals $ \v -> jhFromList
        [("key", [jmacroE| `nm` |]), ("value", [jmacroE| `v` |])]