{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Applicative
import           Control.Monad               (mzero)
import           Data.Aeson                  (FromJSON (..), Value (..), (.:))
import           Data.List                   (genericLength)
import qualified Data.Text                   as T
import           Data.Text.Format            (fixed, format)
import           Data.Text.Lazy              (Text)
import qualified Data.Text.Lazy              as TL
import           Data.Time                   (UTCTime, formatTime)
import qualified Database.Persist            as DB
import qualified Database.Persist.Postgresql as DB
import           System.Locale               (defaultTimeLocale)
import           Web.Scotty.Trans            (file, get, jsonData, post,
                                              redirect, setHeader, text)

import Model
import WebService

data InputRecord = InputRecord { date :: UTCTime
                               , weight :: Double
                               } deriving (Eq, Show)

instance FromJSON InputRecord where
    parseJSON (Object v) = InputRecord <$> v .: "date" <*> v .: "weight"
    parseJSON _ = mzero

period :: Int
period = 28

average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs

renderDate :: UTCTime -> T.Text
renderDate = T.pack . formatTime defaultTimeLocale "%Y-%m-%d"

mkRecord :: InputRecord -> [Record] -> Record
mkRecord ir rs = Record (date ir) (weight ir) (average (weight ir : map recordWeight rs))

renderRecords :: [Record] -> Text
renderRecords rs = TL.concat (names : map conv rs)
  where
    names = "Date,Weight,Avg\n"
    conv (Record {..}) = format "{},{},{}\n" ( renderDate recordDate
                                             , fixed 2 recordWeight
                                             , fixed 2 recordAvg
                                             )

main :: IO ()
main = runService $ do
    runDB (DB.runMigration migrateAll)
    get "/" $ do
        setHeader "Content-Type" "text/html"
        file "index.html"
    get "/dygraph-combined.js" $ do
        setHeader "Content-Type" "text/javascript"
        file "dygraph-combined.js"
    get "/data.csv" $ do
        setHeader "Content-Type" "text/plain"
        rs <- runDB $ DB.selectList [] [DB.Asc RecordDate]
        text $ renderRecords (map DB.entityVal rs)
    post "/" $ do
        input <- jsonData
        _ <- runDB $ do
            prev <- map DB.entityVal <$> DB.selectList [] [DB.Desc RecordDate, DB.LimitTo period]
            DB.insert $ mkRecord input prev
        redirect "/"
