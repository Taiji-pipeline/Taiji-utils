{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes #-}
module Taiji.Utils.Plot
    ( savePlots
    ) where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Data.Text.Lazy.IO as T

import Taiji.Utils.Plot.ECharts (EChart, embedEchart)
import Taiji.Utils.Plot.Vega (Vega, embedVega)

savePlots :: FilePath -> [Vega] -> [EChart] -> IO ()
savePlots output vega echart = T.writeFile output $ renderHtml $ mkHtml vega echart

mkHtml :: [Vega] -> [EChart] -> H.Html
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
    vega = sequence_ $ zipWith (\i v -> embedVega ("vega" ++ show i) v) [0 :: Int ..] vls
    echart = sequence_ $ zipWith (\i v -> embedEchart ("echart" ++ show i) v) [0 :: Int ..] ec

{-
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
      -}