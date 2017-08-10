{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module MongoTypes.AppAuth where

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

-- User Details
