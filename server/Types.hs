{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson.Types (ToJSON)
import GHC.Generics (Generic)

data Environment
    = Development
    | Production
    deriving (Show)


data EnvironmentVariables = EnvironmentVariables
    { environment   :: Environment
    , port          :: Int
    , dbName        :: String
    , twitterKey    :: String
    , twitterSecret :: String
    }
    deriving (Show)


-- API Types


newtype InfoMsg = InfoMsg { msg :: String }
    deriving (Generic, Show)

instance ToJSON InfoMsg
