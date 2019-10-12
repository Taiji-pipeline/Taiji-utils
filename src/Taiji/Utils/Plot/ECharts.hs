{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes #-}

module Taiji.Utils.Plot.ECharts
    ( module Taiji.Utils.Plot.ECharts.Bar
    , module Taiji.Utils.Plot.ECharts.Tree
    , module Taiji.Utils.Plot.ECharts.Pie
    , module Taiji.Utils.Plot.ECharts.Line
    , module Taiji.Utils.Plot.ECharts.Scatter
    , module Taiji.Utils.Plot.ECharts.PunchChart
    , module Taiji.Utils.Plot.ECharts.Heatmap
    , module Taiji.Utils.Plot.ECharts.Boxplot
    , module Taiji.Utils.Plot.ECharts.Radar
    , module Taiji.Utils.Plot.ECharts.Types
    , module Taiji.Utils.Plot.ECharts.Graph
    , embedEchart
    ) where

import Language.Javascript.JMacro
import qualified Text.Blaze.Html5 as H hiding (style)
import qualified Text.Blaze.Html5.Attributes as H
import Text.Printf (printf)
import Data.String (fromString)

import Taiji.Utils.Plot.ECharts.Bar
import Taiji.Utils.Plot.ECharts.Tree
import Taiji.Utils.Plot.ECharts.Pie
import Taiji.Utils.Plot.ECharts.Line
import Taiji.Utils.Plot.ECharts.Scatter
import Taiji.Utils.Plot.ECharts.PunchChart
import Taiji.Utils.Plot.ECharts.Heatmap
import Taiji.Utils.Plot.ECharts.Boxplot
import Taiji.Utils.Plot.ECharts.Radar
import Taiji.Utils.Plot.ECharts.Graph
import Taiji.Utils.Plot.ECharts.Types

embedEchart :: String   -- ^ ID
            -> EChart -> H.Html
embedEchart eid (EChart opt codes w h r) = H.div $ do
    H.div H.! H.id (H.toValue $ eid ++ "_config") $ mempty
    H.div H.! H.id (H.toValue eid) H.! H.style
        (fromString $ printf "width: %dpx;height:%dpx;" w h) $ mempty
    H.script H.! H.type_ "text/javascript" $ H.toHtml $ show $ renderJs [jmacro|
        (function () {
            var !myChart = echarts.init(document.getElementById(`eid`), null, `renderer`);
            var !config = document.getElementById(`eid++"_config"`);
            `codes`;
            var options = `opt`.reduce(function (res, obj) {
                Object.keys(obj).forEach(function (k) { 
                    if(res[k]) {
                        Object.assign(res[k], obj[k]);
                    } else {
                        res[k] = obj[k];
                    }
                });
                return res;
            }, {});
            myChart.setOption(options);
        })();
    |]
  where
    renderer = case r of
        Canvas -> [jmacroE| {renderer: "canvas"} |]
        SVG -> [jmacroE| {renderer: "svg"} |]