{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Control.Applicative
import           Control.Monad                        (mzero)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.Logger                 (runNoLoggingT,
                                                       runStdoutLoggingT)
import           Control.Monad.Reader                 (MonadReader,
                                                       ReaderT (..), asks)
import           Control.Monad.Trans.Class            (MonadTrans, lift)
import           Data.Aeson                           (FromJSON (..),
                                                       Value (..), object, (.:),
                                                       (.=))
import           Data.Default                         (def)
import           Data.List                            (genericLength)
import qualified Data.Text                            as T
import           Data.Text.Encoding                   (encodeUtf8)
import           Data.Text.Format                     (fixed, format)
import           Data.Text.Lazy                       (Text)
import qualified Data.Text.Lazy                       as TL
import           Network.HTTP.Types.Status            (internalServerError500)
import           Network.Wai                          (Middleware)
import           Network.Wai.Handler.Warp             (Settings,
                                                       defaultSettings,
                                                       setFdCacheDuration,
                                                       setPort)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

import           Data.Time                            (UTCTime, formatTime)
import qualified Database.Persist                     as DB
import qualified Database.Persist.Postgresql          as DB
import           System.Environment                   (lookupEnv)
import           System.Locale                        (defaultTimeLocale)
import           Web.Heroku
import           Web.Scotty.Trans                     (ActionT, Options (..),
                                                       ScottyT, defaultHandler,
                                                       file, get, json,
                                                       jsonData, middleware,
                                                       post, redirect,
                                                       scottyOptsT, setHeader,
                                                       showError, status, text)

import Model

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

data Config = Config { environment :: Environment
                     , pool :: DB.ConnectionPool
                     }

data Environment = Development
                 | Production
                 | Test
                 deriving (Show, Eq, Read)

getConfig :: IO Config
getConfig = do
    environment <- getEnvironment
    pool <- getPool environment
    return Config{..}

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

getConnectionSize :: Environment -> Int
getConnectionSize Production = 4
getConnectionSize _ = 1

newtype ConfigM a = ConfigM { runConfigM :: ReaderT Config IO a }
                  deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

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

type Error = Text

runDB :: (MonadTrans t, MonadIO (t ConfigM))
      => DB.SqlPersistT IO a
      -> t ConfigM a
runDB q = do
    p <- lift (asks pool)
    liftIO (DB.runSqlPool q p)

loggingM :: Environment -> Middleware
loggingM Development = logStdoutDev
loggingM Production  = logStdout
loggingM Test        = id

type Action = ActionT Error ConfigM ()

defaultH :: Environment -> Error -> Action
defaultH e x = do
    status internalServerError500
    let o = case e of
                Production -> Null
                _          -> object ["error" .= showError x]
    json o

application :: ScottyT Error ConfigM ()
application = do
    runDB (DB.runMigration migrateAll)
    e <- lift (asks environment)
    middleware (loggingM e)
    defaultHandler (defaultH e)
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

runApplication :: Config -> IO ()
runApplication c = do
    o <- getOptions (environment c)
    let r m = runReaderT (runConfigM m) c
    scottyOptsT o r r application

main :: IO ()
main = do
    c <- getConfig
    runApplication c
