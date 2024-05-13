{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Handlers where

import Yesod
import Foundation
import Network.Wai (Request(..))
import ClassyPrelude hiding (Handler)

data ApiVersion = V1 | V2 deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

newtype EchoResp = EchoResp ApiVersion deriving (Eq, Show, Generic, ToJSON)

postEchoR :: Handler Value
postEchoR = do
  req <- waiRequest
  let headers = requestHeaders req
      apiVersion = lookup "Api-Version" headers
  case apiVersion of
    (Just "v1") -> returnJson $ EchoResp V1
    (Just "v2") -> returnJson $ EchoResp V2
    _ -> invalidArgs ["Api-Version"]

getCompaniesR :: Int -> Handler Value
getCompaniesR companyId = undefined
