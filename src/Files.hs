{-# LANGUAGE TemplateHaskell #-}
module Files where

import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.FileEmbed

dygraphs, index, moment, admin, style, dashboard, helpers :: ByteString
dygraphs = fromStrict $(embedFile "src/js/dygraph-combined.js")
index = fromStrict $(embedFile "src/html/index.html")
moment = fromStrict $(embedFile "src/js/moment.min.js")
admin = fromStrict $(embedFile "src/html/admin.html")
dashboard = fromStrict $(embedFile "src/html/dashboard.html")
style = fromStrict $(embedFile "src/css/style.css")
helpers = fromStrict $(embedFile "src/js/helpers.js")
