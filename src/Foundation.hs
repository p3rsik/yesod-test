{-# LANGUAGE MultiParamTypeClasses #-}
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

import Model
import Settings (AppSettings (..))
import Yesod.Auth (Auth, AuthenticationResult (..), Creds (..), YesodAuth (..), YesodAuthPersist, getAuth)
import Yesod.Auth.HashDB (authHashDB)
import Yesod.Auth.Message (AuthMessage (..))

data App = App
    { appSettings :: AppSettings
    , appConnPool :: ConnectionPool
    , appHttpManager :: Manager
    , appLogger :: Logger
    }

mkYesodData
    "App"
    [parseRoutes| 
/ HomeR GET
/api/echo EchoR POST
/api/users/#Int UsersR GET 
/api/auth AuthR Auth getAuth
|]

instance Yesod App where
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root
    isAuthorized (UsersR _) False = hasToken
    isAuthorized _ _ = pure Authorized

hasToken :: HandlerFor App AuthResult
hasToken = do
    mu <- maybeAuthId
    pure $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodAuth App where
    type AuthId App = UserId
    loginDest _ = HomeR
    logoutDest _ = HomeR
    redirectToReferer _ = False
    authPlugins _ = [authHashDB (Just . UniqueName)]
    authenticate creds = liftHandler $ runDB $ do
        x <- getBy $ UniqueName $ credsIdent creds
        case x of
            Nothing -> return $ UserError InvalidLogin
            Just (Entity uid _) -> return $ Authenticated uid

-- maybeAuthId =

instance YesodAuthPersist App
