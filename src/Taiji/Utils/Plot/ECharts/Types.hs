{-# LANGUAGE QuasiQuotes #-}
module Taiji.Utils.Plot.ECharts.Types
    ( EChart(..)
    , EChartAttribute(..)
    , Renderer(..)
    , mkEChart
    , setDim

    , toolbox
    , title
    , xAxisLabel
    , yAxisLabel

    , symbols
    , defColors
    ) where

import Language.Javascript.JMacro

data EChart = EChart
    { _option :: [JExpr]
    , _js_codes :: JStat
    , _width :: Int
    , _height :: Int
    , _renderer :: Renderer
    }

data Renderer = Canvas | SVG

class EChartAttribute a where
    addAttr :: a -> EChart -> EChart
    (>+>) :: a -> EChart -> EChart
    (>+>) = addAttr

    infixr 6 >+>


instance EChartAttribute JExpr where
    addAttr expr e = e{_option=_option e ++ [expr]}

instance EChartAttribute JStat where
    addAttr js e = e{_js_codes=_js_codes e <> js}

mkEChart :: JExpr -> EChart
mkEChart expr = EChart [expr] mempty 1300 700 Canvas

setDim :: Int -> Int -> EChart -> EChart
setDim w h e = e{_width=w, _height=h}

toolbox :: JExpr
toolbox = [jmacroE| {
    toolbox: {
        show: true,
        feature: {
            restore: {},
            dataZoom: {},
            dataView: {},
            saveAsImage: {
                pixelRatio: 3,
                excludeComponents: ["dataZoom", "timeline", "toolbox"]
            }
        }
    }
    }|]

title :: String -> JExpr
title x = [jmacroE| { title: {text: `x`} } |]

yAxisLabel :: String -> JExpr
yAxisLabel x = [jmacroE| {
    yAxis: {
        name: `x`,
        nameLocation: "middle",
        nameGap: 40
    }
    }|]

xAxisLabel :: String -> JExpr
xAxisLabel x = [jmacroE| {
    xAxis: {
        name: `x`,
        nameLocation: "middle"
    }
    }|]

symbols :: [String]
symbols = ["circle", "rect", "roundRect", "triangle", "diamond", "pin", "arrow"]

defColors :: [String]
defColors = [ "#ff7f50", "#87cefa", "#da70d6", "#32cd32", "#6495ed", 
    "#ff69b4", "#ba55d3", "#cd5c5c", "#ffa500", "#40e0d0", 
    "#1e90ff", "#ff6347", "#7b68ee", "#00fa9a", "#ffd700", 
    "#6b8e23", "#ff00ff", "#3cb371", "#b8860b", "#30e0e0" ]