{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes #-}
module Taiji.Utils.Plot
    ( savePlots
    , vegaBar
    , vegaStackBar
    , vegaViolin
    ) where

import Data.Aeson.QQ (aesonQQ)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Graphics.Vega.VegaLite hiding (lookup)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H
import qualified Data.HashMap.Strict as M
import Text.Printf (printf)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text as T

import Taiji.Utils.Plot.ECharts (EChart, embedEchart)

savePlots :: FilePath -> [VLSpec] -> [EChart] -> IO ()
savePlots output vega echart = T.writeFile output $ renderHtml $ mkHtml vega echart

mkHtml :: [VLSpec] -> [EChart] -> H.Html
mkHtml vls ec = H.docTypeHtml $ do
    H.head $ do
        H.script H.! H.src "https://cdn.jsdelivr.net/npm/vega@5" $ mempty
        H.script H.! H.src "https://cdn.jsdelivr.net/npm/vega-lite@3" $ mempty
        H.script H.! H.src "https://cdn.jsdelivr.net/npm/vega-embed@4" $ mempty
        H.script H.! H.src "https://cdnjs.cloudflare.com/ajax/libs/echarts/4.2.1/echarts.min.js" $ mempty
        H.script H.! H.src "https://cdn.jsdelivr.net/npm/echarts-gl@1.1.1/dist/echarts-gl.min.js" $ mempty
    H.body $ do
      H.div vega
      H.div echart
  where
    vega = sequence_ $ zipWith (\i v -> vegaEmbed ("vega" ++ show i) v) [0..] vls
    echart = sequence_ $ zipWith (\i v -> embedEchart ("echart" ++ show i) v) [0..] ec

vegaEmbed :: String   -- ^ id
          -> VLSpec
          -> H.Html
vegaEmbed vid vega = H.div $ do
    H.div H.! H.id (H.toValue vid) $ return ()
    H.script H.! H.type_ "text/javascript" $ H.toHtml (content :: String)
  where
    content = printf "vegaEmbed('#%s', %s).then(function(result) {}).catch(console.error);"
        vid (B.unpack $ encode vega)

vegaBar :: T.Text   -- ^ title
        -> T.Text   -- ^ X label
        -> T.Text   -- ^ Y label
        -> [(T.Text, Double)]
        -> VLSpec
vegaBar t xl yl dat = fromVL $ toVegaLite
    [ title t
    , background "white"
    , dataFromColumns [] $
        dataColumn xl (Strings xs) $
        dataColumn yl (Numbers ys) []
    , mark Bar []
    , height 300
    , enc ]
  where
    (xs, ys) = unzip dat
    enc = encoding $
        position X [PName xl, PmType Nominal] $
        position Y [PName yl, PmType Quantitative] []

vegaStackBar :: T.Text   -- ^ title
             -> T.Text   -- ^ X label
             -> T.Text   -- ^ Y label
             -> [T.Text]  -- ^ X data
             -> [(T.Text, [Double])]  -- ^ Y data
             -> VLSpec
vegaStackBar t xl yl xdat ydat = 
    let dat = flip concatMap ydat $ \(c, vs) -> flip map (zip xdat vs) $ \(x, y) ->
            M.fromList [(xl, toJSON x), (yl, toJSON y), ("category", toJSON c)]
    in [aesonQQ|
      { "$schema": "https://vega.github.io/schema/vega-lite/v3.json",
        "title": #{t},
        "data": {
          "values": #{dat}
        },
        "mark": "bar",
        "encoding": {
          "x": {"field": #{xl}, "type": "nominal"},
          "y": {
            "aggregate": "sum",
            "field": #{yl},
            "type": "quantitative",
            "axis": { "title": #{yl} }
          },
          "color": {"field": "category", "type": "nominal"}
        }
      } |]


vegaViolin :: [(T.Text, [Double])] -> VLSpec
vegaViolin input =
    let input' = flip concatMap input $ \(x,ys) -> flip map ys $ \y ->
            M.fromList [("key"::T.Text, toJSON x), ("value", toJSON y)]
    in [aesonQQ|
      { "$schema": "https://vega.github.io/schema/vega/v5.json",
        "width": 500,
        "padding": 5,

        "config": {
          "axisBand": {
            "bandPosition": 1,
            "tickExtra": true,
            "tickOffset": 0
          }
        },

        "signals": [
          { "name": "fields",
            "value": #{ fst $ unzip input } },
          { "name": "plotWidth", "value": 60 },
          { "name": "height", "update": "(plotWidth + 10) * length(fields)"},
          { "name": "trim", "value": true,
            "bind": {"input": "checkbox"} },
          { "name": "bandwidth", "value": 0.3,
            "bind": {"input": "range", "min": 0, "max": 1.0, "step": 0.01} }
        ],

  "data": [
    {
      "name": "iris",
      "values": #{ input' }
    },
    {
      "name": "density",
      "source": "iris",
      "transform": [
        {
          "type": "kde",
          "field": "value",
          "groupby": ["key"],
          "bandwidth": {"signal": "bandwidth"},
          "extent": {"signal": "trim ? null : [0, 8]"}
        }
      ]
    },
    {
      "name": "stats",
      "source": "iris",
      "transform": [
        {
          "type": "aggregate",
          "groupby": ["key"],
          "fields": ["value", "value", "value"],
          "ops": ["q1", "median", "q3"],
          "as": ["q1", "median", "q3"]
        }
      ]
    }
  ],

  "scales": [
    {
      "name": "layout",
      "type": "band",
      "range": "height",
      "domain": {"data": "iris", "field": "key"}
    },
    {
      "name": "xscale",
      "type": "linear",
      "range": "width", "round": true,
      "domain": {"data": "iris", "field": "value"},
      "zero": true, "nice": true
    },
    {
      "name": "hscale",
      "type": "linear",
      "range": [0, {"signal": "plotWidth"}],
      "domain": {"data": "density", "field": "density"}
    },
    {
      "name": "color",
      "type": "ordinal",
      "domain": {"data": "iris", "field": "key"},
      "range": "category"
    }
  ],

  "axes": [
    {"orient": "bottom", "scale": "xscale", "zindex": 1},
    {"orient": "left", "scale": "layout", "tickCount": 5, "zindex": 1}
  ],

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