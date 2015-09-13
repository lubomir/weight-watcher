{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Applicative
import           Data.Aeson                  (object, (.=))
import           Data.Maybe                  (maybeToList)
import           Data.Text                   (Text)
import qualified Data.Text.Lazy              as TL
import qualified Database.Persist            as DB
import qualified Database.Persist.Postgresql as DB
import           Network.HTTP.Types.Status   (created201, forbidden403,
                                              status204, status404)
import           Web.Scotty.Trans

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

-- |Get optional parameter. This can only be reasonably used for query
-- parameters.
maybeParam :: (Parsable a, ScottyError e, Functor m, Monad m)
           => TL.Text -> ActionT e m (Maybe a)
maybeParam name = (Just <$> param name) `rescue` \_ -> return Nothing

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
        msince <- maybeParam "after"
        let filters = maybeToList $ (RecordDate DB.>.) <$> msince
        rs <- runDB $ DB.selectList filters [DB.Asc RecordDate]
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
