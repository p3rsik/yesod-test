{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ViewPatterns #-}

module Application (appMain) where

import Control.Monad (when)
import Control.Monad.Logger (liftLoc, runLoggingT)
import Data.Default (def)
import Database.Persist.Sqlite (
    createSqlitePool,
    runMigration,
    runSqlPool,
    sqlDatabase,
    sqlPoolSize,
 )
import Language.Haskell.TH.Syntax (qLocation)
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (
    Settings,
    defaultSettings,
    defaultShouldDisplayException,
    runSettings,
    setHost,
    setOnException,
    setPort,
 )
import Network.Wai.Middleware.RequestLogger (Destination (Logger), destination, mkRequestLogger)
import System.Log.FastLogger (
    defaultBufSize,
    newStdoutLoggerSet,
    toLogStr,
 )
import Yesod
import Yesod.Core.Types (loggerSet)
import Yesod.Default.Config2

import Foundation
import Model
import Settings (AppSettings (..), configSettingsYmlValue)
import Handlers

mkYesodDispatch "App" resourcesApp

makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    appHttpManager <- getGlobalManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger

    let mkFoundation appConnPool = App{..}
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger

    pool <-
        flip runLoggingT logFunc $
            createSqlitePool
                (sqlDatabase $ appDbConf appSettings)
                (sqlPoolSize $ appDbConf appSettings)

    runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc

    pure $ mkFoundation pool

-- | Make a Wai application from our 'App'
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    appPlain <- toWaiAppPlain foundation
    pure $ logWare $ defaultMiddlewaresNoLogging appPlain

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
    mkRequestLogger
        def
            { destination = Logger $ loggerSet $ appLogger foundation
            }

-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
    setPort (appPort $ appSettings foundation) $
        setHost (appHost $ appSettings foundation) $
            setOnException
                ( \_req e ->
                    when (defaultShouldDisplayException e) $
                        messageLoggerSource
                            foundation
                            (appLogger foundation)
                            $(qLocation >>= liftLoc)
                            "yesod"
                            LevelError
                            (toLogStr $ "Exception from Warp: " <> show e)
                )
                defaultSettings

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <-
        loadYamlSettingsArgs
            -- fall back to compile-time values, set to [] to require values at runtime
            [configSettingsYmlValue]
            -- allow environment variables to override
            useEnv

    -- Generate the foundation from the settings
    foundation <- makeFoundation settings

    -- Generate a WAI Application from the foundation
    app <- makeApplication foundation

    -- Run the application with Warp
    runSettings (warpSettings foundation) app
