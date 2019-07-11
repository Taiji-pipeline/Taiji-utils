{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes #-}

module Taiji.Utils.Plot.ECharts
    ( module Taiji.Utils.Plot.ECharts.Bar
    , module Taiji.Utils.Plot.ECharts.Line
    , module Taiji.Utils.Plot.ECharts.Scatter
    , module Taiji.Utils.Plot.ECharts.PunchChart
    , module Taiji.Utils.Plot.ECharts.Heatmap
    , module Taiji.Utils.Plot.ECharts.Types
    , embedEchart
    ) where

import Language.Javascript.JMacro
import qualified Text.Blaze.Html5 as H hiding (style)
import qualified Text.Blaze.Html5.Attributes as H

import Taiji.Utils.Plot.ECharts.Bar
import Taiji.Utils.Plot.ECharts.Line
import Taiji.Utils.Plot.ECharts.Scatter
import Taiji.Utils.Plot.ECharts.PunchChart
import Taiji.Utils.Plot.ECharts.Heatmap
import Taiji.Utils.Plot.ECharts.Types

embedEchart :: String   -- ^ ID
            -> EChart -> H.Html
embedEchart eid (EChart opt codes) = H.div $ do
    H.div H.! H.id (H.toValue eid) H.! H.style "width: 1200px;height:700px;" $ mempty
    H.script H.! H.type_ "text/javascript" $ H.toHtml $ show $ renderJs [jmacro|
        var myChart = echarts.init(document.getElementById(`eid`));
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
    |]