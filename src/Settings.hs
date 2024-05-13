{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Settings (AppSettings (..), configSettingsYmlBS, configSettingsYmlValue) where

import ClassyPrelude.Yesod
import qualified Control.Exception as Exception
import Data.Aeson (withObject, (.:?))
import Data.FileEmbed (embedFile)
import Data.Yaml (decodeEither')
import Database.Persist.Sqlite (SqliteConf)
import Network.Wai.Handler.Warp (HostPreference)
import Yesod.Default.Config2 (configSettingsYml)

data AppSettings = AppSettings
    { appRoot :: Maybe Text
    , appDbConf :: SqliteConf
    , appPort :: Int
    , appHost :: HostPreference
    }

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        appRoot <- o .:? "approot"
        appDbConf <- o .: "database"
        appPort <- o .: "port"
        appHost <- fromString <$> o .: "host"

        pure AppSettings{..}

configSettingsYmlBS :: ByteString
configSettingsYmlBS = $(embedFile configSettingsYml)

configSettingsYmlValue :: Value
configSettingsYmlValue = either Exception.throw id $ decodeEither' configSettingsYmlBS
