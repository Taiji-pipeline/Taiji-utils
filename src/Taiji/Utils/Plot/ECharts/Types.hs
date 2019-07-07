{-# LANGUAGE QuasiQuotes #-}
module Taiji.Utils.Plot.ECharts.Types
    ( EChart(..)
    , option
    , js
    , toolbox
    , title

    , symbols
    , defColors
    ) where

import Language.Javascript.JMacro

data EChart = EChart
    { _option :: [JExpr]
    , _js_codes :: JStat }

instance Semigroup EChart where
    (EChart a1 b1) <> (EChart a2 b2) = EChart (a1 <> a2) (b1 <> b2)

instance Monoid EChart where
    mempty = EChart [] mempty

option :: JExpr -> EChart
option o = EChart [o] mempty

js :: JStat -> EChart
js j = EChart [] j

toolbox :: EChart
toolbox = option [jmacroE| {
    toolbox: {
        show: true,
        feature: {
            restore: {},
            dataZoom: {},
            saveAsImage: {
                pixelRatio: 3,
                excludeComponents: ["dataZoom", "timeline", "toolbox"]
            }
        }
    }
    }|]

title :: String -> EChart
title x = option [jmacroE| { title: {text: `x`} } |]

symbols :: [String]
symbols = ["circle", "rect", "roundRect", "triangle", "diamond", "pin", "arrow"]

defColors :: [String]
defColors = [ "#ff7f50", "#87cefa", "#da70d6", "#32cd32", "#6495ed", 
    "#ff69b4", "#ba55d3", "#cd5c5c", "#ffa500", "#40e0d0", 
    "#1e90ff", "#ff6347", "#7b68ee", "#00fa9a", "#ffd700", 
    "#6b8e23", "#ff00ff", "#3cb371", "#b8860b", "#30e0e0" ]