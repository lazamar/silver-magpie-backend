{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}

module Types where

import Control.Monad.Database (MonadDB)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Error.Class (MonadError)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Text (Text)
import Database.MongoDB (Action)
import GHC.Generics (Generic)
import Servant.Server (ServerError)

data Environment
    = Development
    | Production
    deriving (Show, Generic)

instance FromJSON Environment

data EnvironmentVariables = EnvironmentVariables
    { environment   :: Environment
    , port          :: Int
    , domain        :: Text
    , dbName        :: Text
    , dbUrl         :: Text
    , dbPort        :: Int
    , dbUsername    :: Text
    , dbPassword    :: Text
    , twitterKey    :: Text
    , twitterSecret :: Text
    }
    deriving (Show, Generic)

instance FromJSON EnvironmentVariables

-- API Types


newtype InfoMsg = InfoMsg { status :: String }
    deriving (Generic, Show)

instance ToJSON InfoMsg

type DBActionRunner m = (forall a. Action m a -> m a)

type HandlerM m = (MonadDB m, MonadIO m, MonadError ServerError m)
