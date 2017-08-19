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
import qualified Data.Bson as Bson
import Data.Monoid ((<>))
import qualified Data.Text as T


collectionName :: T.Text
collectionName =
    "app-authorisation"


keyAppSessionId :: T.Text
keyAppSessionId =
    "app_session_id"


keyAccessRequestToken :: T.Text
keyAccessRequestToken =
    "access_request_token"


data AppAuth = AppAuth
    { appSessionId       :: String
    , accessRequestToken :: String
    }
    deriving (Show)


instance FromJSON AppAuth where
    parseJSON = withObject "AppAuth" $ \v -> AppAuth
        <$> v .: keyAppSessionId
        <*> v .: keyAccessRequestToken


instance ToJSON AppAuth where
    -- this generates a Value
    toJSON (AppAuth sId aToken) =
        object
            [ keyAppSessionId .= sId
            , keyAccessRequestToken .= aToken
            ]

    -- this encodes directly to a bytestring Builder
    toEncoding (AppAuth sId aToken) =
        pairs
            ( keyAppSessionId .= sId
            <> keyAccessRequestToken .= aToken
            )


fromBSON :: Bson.Document -> Either String AppAuth
fromBSON doc =
    do
        sessionId     <- getProp (keyAppSessionId::T.Text)
        requestToken  <- getProp (keyAccessRequestToken::T.Text)
        return
            AppAuth
                { appSessionId = sessionId
                , accessRequestToken = requestToken
                }
    where
        getProp name =
            maybe
                (Left $ "No " ++ T.unpack name ++ " field in AppAuth.")
                Right
                (Bson.lookup name doc :: Maybe String)
