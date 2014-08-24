{-# LANGUAGE CPP, OverloadedStrings, FlexibleContexts, ExplicitForAll #-}

module DBUtils where

--import           Control.Applicative         ((<$>))
--import           Database.Persist
import           Database.Persist.Sql
--import Control.Monad.Trans.Resource (runResourceT, ResourceT)
--import           Control.Monad.Logger                 (runNoLoggingT, NoLoggingT)


--import           Model

-- #define SQLite

#ifdef SQLite

import           Control.Monad.IO.Class      (MonadIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Database.Persist.Sqlite

connectDB :: (MonadBaseControl IO m, MonadIO m) => (ConnectionPool -> m a) -> m a
connectDB = withSqlitePool "db.sqlite" 1


#else

import           Data.Monoid                 ((<>))
import           Data.Text.Encoding          (encodeUtf8)
import           Database.Persist.Postgresql (withPostgresqlPool)
import           Web.Heroku                  (dbConnParams)

connectDB :: forall a. (ConnectionPool -> IO a) -> IO a
connectDB act = do
    params <- dbConnParams
    let connStr = foldr (\(k,v) t -> t <> encodeUtf8 (k <> "=" <> v <> " "))
                  "" params
    withPostgresqlPool connStr 1 act

#endif

--readData :: IO [Record]
--readData = map entityVal <$> (runSql $ selectList [] [Asc RecordDate])
--
--addRecord :: Record -> IO ()
--addRecord rec = runSql $ insert rec >> return ()
