{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes #-}

module Taiji.Utils.Plot.ECharts.Tree (tree) where

import Language.Javascript.JMacro
import Data.Aeson
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M
import Data.Tree

import Taiji.Utils.Plot.ECharts.Types

fromTree :: Tree T.Text -> Value
fromTree (Node label []) = toJSON $ M.fromList [("name" :: T.Text, toJSON label)]
fromTree (Node label children) = toJSON $ M.fromList
    [ ("name" :: T.Text, toJSON label)
    , ("children", toJSON $ map fromTree children) ]

tree :: Tree T.Text -> EChart
tree input = mkEChart [jmacroE| {
    series: {
        type: "tree",
        data: [`fromTree input`],
        symbolSize: 7,
        initialTreeDepth: -1,
        label: {
            normal: {
                position: "left",
                verticalAlign: "middle",
                align: "right",
                fontSize: 9
            }
        },

        leaves: {
            label: {
                normal: {
                    position: "right",
                    verticalAlign: "middle",
                    align: "left"
                }
            }
        },
        animationDuration: 550,
        animationDurationUpdate: 750
    }
    } |]