{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Database.Persist.Sql (ConnectionPool, SqlBackend, runSqlPool)
import Network.HTTP.Client (Manager)
import Yesod
import Yesod.Core.Types (Logger)

import Settings (AppSettings (..))

data App = App
    { appSettings :: AppSettings
    , appConnPool :: ConnectionPool
    , appHttpManager :: Manager
    , appLogger :: Logger
    }

mkYesodData
    "App"
    [parseRoutes| 
/api/echo EchoR POST
/api/companies/#Int CompaniesR GET 
|]

instance Yesod App where
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
