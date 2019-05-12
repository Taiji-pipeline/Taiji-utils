{-# LANGUAGE OverloadedStrings     #-}
module Taiji.Utils.Plot
    ( savePlots
    , vegaBar
    ) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Graphics.Vega.VegaLite hiding (lookup)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H
import Text.Printf (printf)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text as T

savePlots :: FilePath -> [VLSpec] -> IO ()
savePlots output = T.writeFile output . renderHtml . mkHtml 

mkHtml :: [VLSpec] -> H.Html
mkHtml vls = H.docTypeHtml $ do
    H.head $ do
        H.script H.! H.src "https://cdn.jsdelivr.net/npm/vega@5" $ mempty
        H.script H.! H.src "https://cdn.jsdelivr.net/npm/vega-lite@3" $ mempty
        H.script H.! H.src "https://cdn.jsdelivr.net/npm/vega-embed@4" $ mempty
    H.body vega
  where
    vega = mconcat $ zipWith (\i v -> vegaEmbed ("vega" ++ show i) v) [0..] vls

vegaEmbed :: String   -- ^ id
          -> VLSpec
          -> H.Html
vegaEmbed vid vega = do
    H.div H.! H.id (H.toValue vid) $ return ()
    H.script H.! H.type_ "text/javascript" $ H.toHtml (content :: String)
  where
    content = printf "vegaEmbed('#%s', %s).then(function(result) {}).catch(console.error);"
        vid (B.unpack $ encode vega)

vegaBar :: T.Text   -- ^ title
        -> T.Text   -- ^ X label
        -> T.Text   -- ^ Y label
        -> [(T.Text, Double)]
        -> VLSpec
vegaBar t xl yl dat = fromVL $ toVegaLite
    [ title t
    , background "white"
    , dataFromColumns [] $
        dataColumn xl (Strings xs) $
        dataColumn yl (Numbers ys) []
    , mark Bar []
    , height 300
    , enc ]
  where
    (xs, ys) = unzip dat
    enc = encoding $
        position X [PName xl, PmType Nominal] $
        position Y [PName yl, PmType Quantitative] []

