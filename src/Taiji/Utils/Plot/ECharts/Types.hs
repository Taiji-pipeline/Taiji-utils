{-# LANGUAGE QuasiQuotes #-}
module Taiji.Utils.Plot.ECharts.Types
    ( EChart(..)
    , option
    , js
    , toolbox
    , title
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

