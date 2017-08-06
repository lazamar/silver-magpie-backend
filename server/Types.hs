{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Types where

import Data.Aeson.Types
    ( FromJSON
    , ToJSON
    , object
    , pairs
    , parseJSON
    , toEncoding
    , toJSON
    , withObject
    , (.:)
    , (.=)
    )
import Data.Monoid ((<>))
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
    , twitterKey    :: Text
    , twitterSecret :: Text
    }
    deriving (Show, Generic)

instance FromJSON EnvironmentVariables

-- API Types


newtype InfoMsg = InfoMsg { msg :: String }
    deriving (Generic, Show)

instance ToJSON InfoMsg

type DBActionRunner = (forall a. Action IO a -> IO a)

-- Mongo Types

data AppAuth = AppAuth
    { appSessionId       :: String
    , accessRequestToken :: String
    }
    deriving (Show)

instance FromJSON AppAuth where
    parseJSON = withObject "AppAuth" $ \v -> AppAuth
        <$> v .: "app_session_id"
        <*> v .: "access_request_token"

instance ToJSON AppAuth where
    -- this generates a Value
    toJSON (AppAuth sId aToken) =
        object
            [ "app_session_id" .= sId
            , "access_request_token" .= aToken
            ]

    -- this encodes directly to a bytestring Builder
    toEncoding (AppAuth sId aToken) =
        pairs
            ( "app_session_id" .= sId
            <> "access_request_token" .= aToken
            )
