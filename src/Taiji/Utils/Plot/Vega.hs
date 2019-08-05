{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes #-}

module Taiji.Utils.Plot.Vega
    ( module Taiji.Utils.Plot.Vega.Heatmap
    , module Taiji.Utils.Plot.Vega.Histogram
    , module Taiji.Utils.Plot.Vega.Violin
    , module Taiji.Utils.Plot.Vega.Types
    , embedVega
    ) where

import Language.Javascript.JMacro
import qualified Text.Blaze.Html5 as H hiding (style)
import qualified Text.Blaze.Html5.Attributes as H

import Taiji.Utils.Plot.Vega.Types
import Taiji.Utils.Plot.Vega.Heatmap
import Taiji.Utils.Plot.Vega.Violin
import Taiji.Utils.Plot.Vega.Histogram

embedVega :: String   -- ^ ID
          -> Vega -> H.Html
embedVega eid (Vega vega) = H.div $ do
    H.div H.! H.id (H.toValue eid) $ return ()
    H.script H.! H.type_ "text/javascript" $ H.toHtml $ show $ renderJs [jmacro|
        function isObject(obj) {
            return (typeof obj === "object" && obj !== null) || typeof obj === "function";
        }
        function deepMerge(target, source) {
            if (Array.isArray(target) && !Array.isArray(source)) {
                target.push(source);
                return target;
            } else if (isObject(target) && isObject(source)) {
                Object.keys(source).forEach(function (k) { 
                    if(target[k]) {
                        target[k] = deepMerge(target[k], source[k]);
                    } else {
                        target[k] = source[k];
                    }
                });
                return target;
            } else {
                return source;
            }
        }
        var options = `vega`.reduce(deepMerge, {});
        vegaEmbed(`'#' : eid`, options);
        |]