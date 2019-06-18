{-# LANGUAGE OverloadedStrings     #-}

module Taiji.Utils.Plot.ECharts.Types where

import qualified Data.Text.Lazy as TL
import qualified Text.Blaze.Html5 as H hiding (style)
import qualified Text.Blaze.Html5.Attributes as H

newtype EChart = EChart (String -> TL.Text)

embedEchart :: String -> EChart -> H.Html
embedEchart eid (EChart e) = H.div $ do
    H.div H.! H.id (H.toValue eid) H.! H.style "width: 1200px;height:700px;" $ mempty
    H.script H.! H.type_ "text/javascript" $ H.toHtml $ e eid

