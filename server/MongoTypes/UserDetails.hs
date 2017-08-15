{-# LANGUAGE OverloadedStrings #-}

module MongoTypes.UserDetails where

import Data.Aeson.Types
    (FromJSON, ToJSON, object, parseJSON, toJSON, withObject, (.:), (.=))
import Data.Bson (Field ((:=)), Value (String))
import qualified Data.Bson as Bson
import qualified Data.Text as T


data UserDetails = UserDetails
    { oauthToken         :: String
    , oauthTokenSecret   :: String
    , userId             :: String
    , screenName         :: String
    , accessRequestToken :: String
    } deriving (Show)


instance FromJSON UserDetails where
    parseJSON = withObject "UserDetails" $ \v -> UserDetails
        <$> v .: "oauth_token"
        <*> v .: "oauth_token_secret"
        <*> v .: "user_id"
        <*> v .: "screen_name"
        <*> v .: "access_request_token"


instance ToJSON UserDetails where
    -- this generates a Value
    toJSON (UserDetails oT oTS uId sN aRT) =
        object
        [ "oauth_token" .= oT
        , "oauth_token_secret" .= oTS
        , "user_id" .= uId
        , "screen_name" .= sN
        , "access_request_token" .= aRT
        ]


toBSON :: UserDetails -> Bson.Document
toBSON (UserDetails oT oTS uId sN aRT) =
    [ "oauth_token" := (String $ T.pack oT)
    , "oauth_token_secret" := (String $ T.pack oTS)
    , "user_id" := (String $ T.pack uId)
    , "screen_name" := (String $ T.pack sN)
    , "access_request_token" := (String $ T.pack aRT)
    ]


fromBSON :: Bson.Document -> Either String UserDetails
fromBSON doc =
    let
        getProp name =
            maybe
                (Left $ "No " ++ T.unpack name ++ " field in credential.")
                Right
                (Bson.lookup name doc :: Maybe String)
    in
        do
            token       <- getProp ("oauth_token"::T.Text)
            tokenSecret <- getProp ("oauth_token_secret"::T.Text)
            uId         <- getProp ("user_id"::T.Text)
            sName       <- getProp ("screen_name"::T.Text)
            aRToken     <- getProp ("access_request_token"::T.Text)
            return
                UserDetails
                    { oauthToken = token
                    , oauthTokenSecret = tokenSecret
                    , userId = uId
                    , screenName = sName
                    , accessRequestToken = aRToken
                    }


removeQuotes :: [a] -> [a]
removeQuotes v =
    drop 1 $ take (length v - 1) v
