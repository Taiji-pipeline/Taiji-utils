{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes #-}
module Taiji.Utils.Plot
    ( savePlots
    , viridis
    , npg
    , aaas
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
        H.script H.! H.src "https://cdn.jsdelivr.net/npm/vega-embed@5" $ mempty
        H.script H.! H.src "https://cdn.jsdelivr.net/npm/echarts@5.0.2/dist/echarts.min.js" $ mempty
        H.script H.! H.src "https://cdn.jsdelivr.net/npm/echarts-gl@2.0.2/dist/echarts-gl.min.js" $ mempty
    H.body $ do
      H.div vega
      H.div echart
  where
    vega = sequence_ $ zipWith (\i v -> embedVega ("vega" ++ show i) v) [0 :: Int ..] vls
    echart = sequence_ $ zipWith (\i v -> embedEchart ("echart" ++ show i) v) [0 :: Int ..] ec

viridis :: [String]
viridis = ["#440154", "#482173", "#433E85", "#38598C", "#2D708E",
    "#25858E", "#1E9B8A", "#2BB07F", "#51C56A", "#85D54A", "#C2DF23", "#FDE725"]

npg :: [String]
npg = [ "#E64B35", "#4DBBD5", "#00A087", "#3C5488", "#F39B7F", "#8491B4", "#91D1C2",
    "#DC0000", "#7E6148", "#B09C85" ]

aaas :: [String]
aaas = [ "#3B4992", "#EE0000", "#008B45", "#631879", "#008280", "#BB0021",
    "#5F559B", "#A20056", "#808180", "#1B1919" ]