{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WebService where

import           Control.Applicative
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.Logger                 (runNoLoggingT,
                                                       runStdoutLoggingT)
import           Control.Monad.Reader                 (MonadReader,
                                                       ReaderT (..), asks)
import           Control.Monad.Trans.Class            (MonadTrans, lift)
import           Data.Aeson                           (Value (..), object, (.=))
import qualified Data.Text                            as T
import           Data.Text.Encoding                   (encodeUtf8)
import           Data.Text.Lazy                       (Text, pack)
import           Network.HTTP.Types.Status            (internalServerError500)
import           Network.Wai                          (Middleware)
import           Network.Wai.Handler.Warp             (Settings,
                                                       defaultSettings,
                                                       setFdCacheDuration,
                                                       setPort)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           Network.Wai.Middleware.Gzip

import qualified Database.Persist.Postgresql          as DB
import           System.Environment                   (lookupEnv)
import           Web.Heroku
import           Web.Scotty.Trans                     (ActionT, Options (..),
                                                       ScottyT, defaultHandler,
                                                       json, middleware, raise,
                                                       scottyOptsT, showError,
                                                       status)

data Config = Config { environment :: Environment
                     , pool :: DB.ConnectionPool
                     }

data Environment = Development
                 | Production
                 | Test
                 deriving (Show, Eq, Read)

newtype ConfigM a = ConfigM { runConfigM :: ReaderT Config IO a }
                  deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

type Error = Text

type Action = ActionT Error ConfigM ()

type Handler = ScottyT Error ConfigM ()

runDB :: (MonadTrans t, MonadIO (t ConfigM))
      => DB.SqlPersistT IO a
      -> t ConfigM a
runDB q = do
    p <- lift (asks pool)
    liftIO (DB.runSqlPool q p)

getConnectionSize :: Environment -> Int
getConnectionSize Production = 4
getConnectionSize _ = 1

getConnectionString :: Environment -> IO DB.ConnectionString
getConnectionString e =
    maybe defConnStr mkConnStr <$> lookupEnv "DATABASE_URL"
  where
    defConnStr = getDefaultConnectionString e
    mkConnStr = createConnectionString . parseDatabaseUrl

getDefaultConnectionString :: Environment -> DB.ConnectionString
getDefaultConnectionString e =
    let n = case e of
                Development -> "weight_watcher_development"
                Production  -> "weight_watcher_production"
                Test        -> "weight_watcher_test"
    in createConnectionString
        [ ("host", "localhost")
        , ("port", "5432")
        , ("user", "postgres")
        , ("dbname", n)
        ]

createConnectionString :: [(T.Text, T.Text)] -> DB.ConnectionString
createConnectionString l = encodeUtf8 (T.unwords (map f l))
  where
    f (k, v) = T.concat [k, "=", v]

loggingM :: Environment -> Middleware
loggingM Development = logStdoutDev
loggingM Production  = logStdout
loggingM Test        = id

defaultH :: Environment -> Error -> Action
defaultH e x = do
    status internalServerError500
    let o = case e of
                Production -> Null
                _          -> object ["error" .= showError x]
    json o

getPort :: IO (Maybe Int)
getPort = fmap read <$> lookupEnv "PORT"

getSettings :: Environment -> IO Settings
getSettings e = do
    let cache = if e == Development then setFdCacheDuration 0 else id
    portSetter <- maybe id setPort <$> getPort
    return $ portSetter $ cache defaultSettings

getOptions :: Environment -> IO Options
getOptions e = do
    s <- getSettings e
    return def { settings = s
               , verbose = case e of Development -> 1
                                     _           -> 0
               }

getEnvironment :: IO Environment
getEnvironment = maybe Development read <$> lookupEnv "SCOTTY_ENV"

getPool :: Environment -> IO DB.ConnectionPool
getPool e = do
    s <- getConnectionString e
    let n = getConnectionSize e
    case e of
        Development -> runStdoutLoggingT (DB.createPostgresqlPool s n)
        Production -> runStdoutLoggingT (DB.createPostgresqlPool s n)
        Test -> runNoLoggingT (DB.createPostgresqlPool s n)

getConfig :: IO Config
getConfig = do
    environment <- getEnvironment
    pool <- getPool environment
    return Config{..}

getAuthToken :: ActionT Error ConfigM (Maybe Text)
getAuthToken = do
    e <- lift (asks environment)
    tok <- liftIO $ lookupEnv "ADMIN_TOKEN"
    case tok of
        Nothing -> if e == Development
                    then return Nothing
                    else raise "Production requires authentication settings."
        Just token -> return $ Just $ pack token

application :: ScottyT Error ConfigM () -> ScottyT Error ConfigM ()
application app = do
    e <- lift (asks environment)
    middleware (loggingM e)
    middleware (gzip $ def {gzipFiles = GzipCompress})
    defaultHandler (defaultH e)
    app

runApplication :: Config -> ScottyT Error ConfigM () -> IO ()
runApplication c app = do
    o <- getOptions (environment c)
    let r m = runReaderT (runConfigM m) c
    scottyOptsT o r r (application app)

runService :: ScottyT Error ConfigM () -> IO ()
runService app = do
    c <- getConfig
    runApplication c app
