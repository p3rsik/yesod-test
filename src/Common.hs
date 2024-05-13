{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Common where

import ClassyPrelude
import ClassyPrelude.Yesod
import Data.Kind (Type)
import Yesod

-- | Simple versioning for the API
data ApiVersion = V1 | V2 deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

newtype EchoResp = EchoResp ApiVersion deriving (Eq, Show, Generic, ToJSON)

type DB a = forall (m :: Type -> Type). (MonadIO m) => ReaderT SqlBackend m a
