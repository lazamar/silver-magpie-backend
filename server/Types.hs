{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson.Types (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Environment
    = Development
    | Production
    deriving (Show)


data EnvironmentVariables = EnvironmentVariables
    { environment   :: Environment
    , port          :: Int
    , dbName        :: Text
    , dbUrl         :: Text
    , twitterKey    :: Text
    , twitterSecret :: Text
    }
    deriving (Show)


-- API Types


newtype InfoMsg = InfoMsg { msg :: String }
    deriving (Generic, Show)

instance ToJSON InfoMsg
