{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.Monoid
import           Data.Text.Lazy              (Text)
import qualified Database.Persist            as DB
import qualified Database.Persist.Postgresql as DB
import           Network.HTTP.Types.Status   (created201)
import           Web.Scotty.Trans            (capture, file, get, json,
                                              jsonData, param, post, setHeader,
                                              status)

import Model
import WebService

serveDir :: FilePath -> Text -> Handler
serveDir dir mime = get (capture $ "/" <> dir <> "/:filename") $ do
    filename <- param "filename"
    setHeader "Content-Type" mime
    file (dir <> "/" <> filename)

main :: IO ()
main = runService $ do
    runDB (DB.runMigration migrateAll)
    get "/" $ do
        setHeader "Content-Type" "text/html"
        file "index.html"
    serveDir "js" "text/javascript"
    serveDir "css" "text/css"
    get "/data.json" $ do
        rs <- runDB $ DB.selectList [] [DB.Asc RecordDate]
        json rs
    post "/" $ do
        input <- jsonData
        runDB $ DB.insert_ (input :: Record)
        status created201
        json input
