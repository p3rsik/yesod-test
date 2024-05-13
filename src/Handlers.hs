{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handlers where

import ClassyPrelude hiding (Handler)
import Network.Wai (Request (..))
import Yesod

import Common
import Foundation

getHomeR :: Handler Value
getHomeR = returnJson @_ @Text "Welcome to my test app!"

postEchoR :: Handler Value
postEchoR = do
    req <- waiRequest
    let headers = requestHeaders req
        apiVersion = lookup "Api-Version" headers
    case apiVersion of
        (Just "v1") -> returnJson $ EchoResp V1
        (Just "v2") -> returnJson $ EchoResp V2
        _ -> invalidArgs ["Api-Version"]

getUsersR :: Int -> Handler Value
getUsersR userId = undefined
