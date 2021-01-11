{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Types where

import Control.Monad.Database (Collection (..), Database (..), MonadDB, Target (..))
import Control.Monad.Database.SQLite (MonadSQL, MonadSQLT)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (ExceptT (..))
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Text (Text)
import Database.MongoDB (Action)
import GHC.Generics (Generic)
import Servant.Server (ServerError)

data Environment
    = Development
    | Production
    deriving (Show , Generic)

instance FromJSON Environment

data EnvironmentVariables = EnvironmentVariables
    { environment :: Environment
    , port :: Int
    , domain :: Text
    , dbName :: Text
    , dbUrl :: Text
    , dbPort :: Int
    , dbUsername :: Text
    , dbPassword :: Text
    , twitterKey :: Text
    , twitterSecret :: Text
    }
    deriving (Show , Generic)

instance FromJSON EnvironmentVariables

-- API Types

newtype InfoMsg = InfoMsg {status :: String}
    deriving (Generic , Show)

instance ToJSON InfoMsg

type DBActionRunner m = (forall a. Action m a -> m a)

type HandlerM m = (MonadDB m , MonadIO m , MonadError ServerError m)

type Stack = MonadSQLT (ExceptT ServerError IO)

targetCollection :: Text -> Target
targetCollection = Target (Database "Noop") . Collection
