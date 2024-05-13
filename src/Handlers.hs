{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import Yesod
import Foundation
import Network.Wai (Request(..))
import ClassyPrelude hiding (Handler)

postEchoR :: Handler Value
postEchoR = do
  req <- waiRequest
  let headers = requestHeaders req
      apiVersion = lookup "Api-Version" headers
  case apiVersion of
    (Just "v1") -> undefined 
    (Just "v2") -> undefined
    _ -> undefined
