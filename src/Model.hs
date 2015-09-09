{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Model where

import           Control.Monad  (mzero)
import           Data.Aeson
import           Data.Text      (Text, pack, unpack)
import           Data.Text.Lazy (toStrict)
import           Data.Time      (Day, parseTime)
import           System.Locale  (defaultTimeLocale)
import           Web.Scotty     (Parsable (..))

import           Database.Persist.TH

instance ToJSON Day where
    toJSON       = String . pack . show

instance FromJSON Day where
    parseJSON = withText "Day" $ maybe mzero return . parse

parse :: Text -> Maybe Day
parse = parseTime defaultTimeLocale "%Y-%m-%d" . unpack

instance Parsable Day where
    parseParam = maybe (Left "no parse") Right . parse . toStrict

share [mkMigrate "migrateAll", mkPersist sqlSettings] [persistLowerCase|
Record json
    date    Day
    weight  Double
    UniqueRecordDate date
    deriving Show
|]
