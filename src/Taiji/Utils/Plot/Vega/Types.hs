{-# LANGUAGE QuasiQuotes #-}
module Taiji.Utils.Plot.Vega.Types
    ( Vega(..)
    , option
    , title
    ) where

import Language.Javascript.JMacro

data Vega = Vega
    { _vega :: [JExpr] }

option :: JExpr -> Vega
option o = Vega [o]

title :: String -> Vega
title x = option [jmacroE| { title: `x` } |]

instance Semigroup Vega where
    (Vega x) <> (Vega y) = Vega (x <> y)

instance Monoid Vega where
    mempty = Vega []