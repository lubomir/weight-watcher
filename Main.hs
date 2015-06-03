{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Applicative
import           Data.Aeson                  (object, (.=))
import           Data.Text                   (Text)
import qualified Database.Persist            as DB
import qualified Database.Persist.Postgresql as DB
import           Network.HTTP.Types.Status   (created201, forbidden403)
import           Web.Scotty.Trans            (file, get, headers, json,
                                              jsonData, post, setHeader, status)

import Model
import WebService

main :: IO ()
main = runService $ do
    runDB (DB.runMigration migrateAll)
    get "/" $ do
        setHeader "Content-Type" "text/html"
        file "index.html"
    get "/js/dygraph-combined.js" $ do
        setHeader "Content-Type" "text/javascript"
        file "js/dygraph-combined.js"
    get "/js/moment.min.js" $ do
        setHeader "Content-Type" "text/javascript"
        file "js/moment.min.js"
    get "/data.json" $ do
        rs <- runDB $ DB.selectList [] [DB.Asc RecordDate]
        json rs
    get "/add" $ do
        setHeader "Content-Type" "text/html"
        file "add.html"
    post "/" $ do
        tok <- lookup "Authorization" <$> headers
        expected <- getAuthToken
        if tok /= expected
            then do
                status forbidden403
                json $ object ["error" .= ("Incorrect auth token." :: Text)]
            else do
                input <- jsonData
                runDB $ DB.insert_ (input :: Record)
                status created201
                json input
