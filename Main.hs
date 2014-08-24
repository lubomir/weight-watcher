{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Applicative
import           Control.Monad                        (mzero)
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Logger                 (runNoLoggingT)
import           Control.Monad.Trans.Resource         (runResourceT)
import           Data.Aeson
import           Data.List                            (genericLength)
import           Data.Text.Format
import qualified Data.Text.Lazy                       as T
import           Data.Time
import           Database.Persist                     hiding (get)
import           Database.Persist.Sql                 hiding (get)
import           Database.Persist.Sqlite              hiding (get)
import           Network.Wai.Middleware.RequestLogger
import           System.Environment
import           System.Locale                        (defaultTimeLocale)
import           Web.Scotty

import Model
--import DBUtils

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

renderRecords :: [Record] -> T.Text
renderRecords rs = T.concat (names : map conv rs)
  where
    names = "Date,Weight,Avg\n"
    conv (Record {..}) = format "{},{},{}\n" ( renderDate recordDate
                                             , fixed 2 recordWeight
                                             , fixed 2 recordAvg
                                             )

main :: IO ()
main = do
    port <- maybe 3000 read <$> lookupEnv "PORT"
    pool <- createSqlitePool "db.sqlite" 1
    runNoLoggingT $ runSqlPool (runMigration migrateAll) pool
    let runDB = liftIO . runNoLoggingT . runResourceT . flip runSqlPool pool

    scotty port $ do
        middleware logStdoutDev
        get "/" $ do
            setHeader "Content-Type" "text/html"
            file "index.html"
        get "/dygraph-combined.js" $ do
            setHeader "Content-Type" "text/javascript"
            file "dygraph-combined.js"
        get "/data.csv" $ do
            setHeader "Content-Type" "text/plain"
            rs <- runDB $ selectList [] [Asc RecordDate]
            text $ renderRecords (map entityVal rs)
        post "/" $ do
            input <- jsonData
            _ <- runDB $ do
                prev <- map entityVal <$> selectList [] [Desc RecordDate, LimitTo period]
                insert $ mkRecord input prev
            redirect "/"
