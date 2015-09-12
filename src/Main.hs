{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Applicative
import           Data.Aeson                  (object, (.=))
import           Data.Text                   (Text)
import qualified Database.Persist            as DB
import qualified Database.Persist.Postgresql as DB
import           Network.HTTP.Types.Status   (created201, forbidden403,
                                              status204, status404)
import           Web.Scotty.Trans            (delete, get, headers, json,
                                              jsonData, param, post, raw,
                                              setHeader, status)

import Files
import Model
import WebService

protected :: Action -> Action
protected act = do
    tok <- lookup "Authorization" <$> headers
    expected <- getAuthToken
    if tok /= expected
        then do
            status forbidden403
            json $ object ["error" .= ("Incorrect auth token." :: Text)]
        else act

main :: IO ()
main = runService $ do
    runDB (DB.runMigration migrateAll)
    get "/" $ do
        setHeader "Content-Type" "text/html"
        raw index
    get "/js/dygraph-combined.js" $ do
        setHeader "Content-Type" "text/javascript"
        raw dygraphs
    get "/js/moment.min.js" $ do
        setHeader "Content-Type" "text/javascript"
        raw moment
    get "/js/helpers.js" $ do
        setHeader "Content-Type" "text/javascript"
        raw helpers
    get "/css/style.css" $ do
        setHeader "Content-Type" "text/css"
        raw style
    get "/data.json" $ do
        rs <- runDB $ DB.selectList [] [DB.Asc RecordDate]
        json rs
    get "/admin" $ do
        setHeader "Content-Type" "text/html"
        raw admin
    get "/dashboard" $ do
        setHeader "Content-Type" "text/html"
        raw dashboard
    post "/add" $ protected $ do
        input <- jsonData
        runDB $ DB.insert_ (input :: Record)
        status created201
        json input
    delete "/delete/:date" $ protected $ do
        date <- param "date"
        mrecord <- runDB $ DB.getBy (UniqueRecordDate date)
        case mrecord of
            Nothing -> do
                status status404
                json $ object ["error" .= ("Record not found." :: Text)]
            Just (DB.Entity key _) -> do
                runDB $ DB.delete key
                status status204
