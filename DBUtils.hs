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

import Prelude hiding (foldr)
import           Data.Monoid                 ((<>))
import           Data.Text.Encoding          (encodeUtf8)
import           Database.Persist.Postgresql (withPostgresqlPool)
import           Data.Text
import           System.Environment
import Network.URI
--import           Web.Heroku                  (dbConnParams)

dbConnParams' :: String -> (String -> [(Text, Text)]) -> IO [(Text, Text)]
dbConnParams' envVar parse = getEnv envVar >>= return . parse

parseDatabaseUrl' :: String -> String -> [(Text, Text)]
parseDatabaseUrl' scheme durl =
  let muri = parseAbsoluteURI durl
      (auth, path) = case muri of
                      Nothing ->  error "couldn't parse absolute uri"
                      Just uri -> if uriScheme uri /= scheme
                                    then schemeError uri
                                    else case uriAuthority uri of
                                           Nothing   -> invalid
                                           Just a -> (a, uriPath uri)
      (user,password) = userAndPassword auth
  in     [ (pack "user",     user)
           -- tail not safe, but should be there on Heroku
         , (pack "password", Data.Text.tail password)
         , (pack "host",     pack $ uriRegName auth)
         , (pack "port",     pack $ removeColon $ uriPort auth)
         -- tail not safe but path should always be there
         , (pack "dbname",   pack $ Prelude.tail $ path)
         ]
  where
    removeColon (':':port) = port
    removeColon port = port

    -- init is not safe, but should be there on Heroku
    userAndPassword :: URIAuth -> (Text, Text)
    userAndPassword = (breakOn $ pack ":") . pack . Prelude.init . uriUserInfo

    schemeError uri = error $ "was expecting a postgres scheme, not: " ++ (uriScheme uri) ++ "\n" ++ (show uri)
    -- should be an error
    invalid = error "could not parse heroku DATABASE_URL"

parseDatabaseUrl :: String -> [(Text, Text)]
parseDatabaseUrl = parseDatabaseUrl' "postgres:"

dbConnParams :: IO [(Text, Text)]
dbConnParams = dbConnParams' "DATABASE_URL" parseDatabaseUrl

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
