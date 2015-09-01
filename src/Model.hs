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

import           Control.Monad (mzero)
import           Data.Aeson
import           Data.Text     (pack, unpack)
import           Data.Time     (Day, parseTime)
import           System.Locale (defaultTimeLocale)

import           Database.Persist.TH

instance ToJSON Day where
    toJSON       = String . pack . show

instance FromJSON Day where
    parseJSON = withText "Day" $
        maybe mzero return . parseTime defaultTimeLocale "%Y-%m-%d" . unpack

share [mkMigrate "migrateAll", mkPersist sqlSettings] [persistLowerCase|
Record json
    date    Day
    weight  Double
    deriving Show
|]
