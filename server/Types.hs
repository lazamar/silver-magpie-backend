{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Types where


import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Text (Text)
import Database.MongoDB (Action)
import GHC.Generics (Generic)

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

type DBActionRunner = (forall a. Action IO a -> IO a)
