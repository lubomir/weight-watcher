{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model where

import           Data.Time           (UTCTime)
import           Database.Persist.TH

share [mkMigrate "migrateAll", mkPersist sqlSettings] [persistLowerCase|
Record json
    date    UTCTime
    weight  Double
    deriving Show
|]
