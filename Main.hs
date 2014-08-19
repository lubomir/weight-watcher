{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isDigit)
import Data.List.Split
import Data.List
import Data.Maybe (fromMaybe)
import Control.Applicative
import Control.Monad (mzero, when)
import Control.Monad.IO.Class (liftIO)
import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import System.Locale (defaultTimeLocale)
import Data.Time
import Data.Aeson
import System.Lock.FLock
import System.Directory
import System.Environment

data Record = Record { date :: UTCTime
                     , weight :: Float
                     } deriving (Eq, Show)

instance FromJSON Record where
    parseJSON (Object v) = do
        datestr <- v .: "date"
        case parseTime defaultTimeLocale "%Y-%m-%d" datestr of
            Nothing -> mzero
            Just d  -> Record d <$> v .: "weight"
    parseJSON _ = mzero


dataFile :: FilePath
dataFile = "weights.csv"

period :: Int
period = 28

lastN :: Int -> [a] -> [a]
lastN n xs = let l = length xs in drop (l - n) xs

average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs

extract :: String -> Float
extract s = case splitOn "," s of
    [_, x, _] -> read x
    _ -> 0

renderDate :: UTCTime -> String
renderDate = formatTime defaultTimeLocale "%Y-%m-%d"

saveData :: FilePath -> Record -> IO ()
saveData fp r = withLock fp Exclusive Block worker
  where
    tmpFile = fp ++ ".out"
    worker = do
        dat <- lines <$> readFile fp
        let prev = (map extract . lastN period . drop 1) dat
        let newAvg = average (weight r : prev) :: Float
        let newLine = intercalate "," [ renderDate $ date r
                                      , show $ weight r
                                      , show newAvg]
        writeFile tmpFile $ unlines (dat ++ [newLine])
        renameFile tmpFile fp

getPort :: IO (Maybe Int)
getPort = do
    args <- getArgs
    case args of
        [mport] -> return $ if (all isDigit mport)
                                then (Just $ read mport)
                                else Nothing
        _ -> return Nothing

main :: IO ()
main = do
    port <- fromMaybe 3000 <$> getPort
    firstRun <- not <$> doesFileExist dataFile
    when firstRun $ writeFile dataFile "Date,Weight,Avg\n"
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
            file dataFile
        post "/" $ do
            input <- jsonData
            liftIO $ saveData dataFile input
            redirect "/"
