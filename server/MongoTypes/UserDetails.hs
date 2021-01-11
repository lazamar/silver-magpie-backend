{-# LANGUAGE OverloadedStrings #-}

module MongoTypes.UserDetails where

import Data.Aeson.Types
    ( FromJSON,
      ToJSON,
      object,
      parseJSON,
      toJSON,
      withObject,
      (.:),
      (.=),
    )
import Data.Bson (Field ((:=)), Value (String))
import qualified Data.Bson as Bson
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite.Simple.ToField as SQL

data UserDetails = UserDetails
    { oauthToken :: String
    , oauthTokenSecret :: String
    , userId :: String
    , screenName :: String
    , accessRequestToken :: String
    }
    deriving (Show)

instance SQL.FromRow UserDetails where
    fromRow =
        UserDetails
            <$> SQL.field
            <*> SQL.field
            <*> SQL.field
            <*> SQL.field
            <*> SQL.field

instance SQL.ToRow UserDetails where
    toRow (UserDetails a b c d e) =
        [ SQL.toField a
        , SQL.toField b
        , SQL.toField c
        , SQL.toField d
        , SQL.toField e
        ]

collectionName :: T.Text
collectionName = "credentials"

keyOauthToken :: T.Text
keyOauthToken =
    "oauth_token"

keyOauthTokenSecret :: T.Text
keyOauthTokenSecret =
    "oauth_token_secret"

keyUserId :: T.Text
keyUserId =
    "user_id"

keyScreenName :: T.Text
keyScreenName =
    "screen_name"

keyAccessRequestToken :: T.Text
keyAccessRequestToken =
    "access_request_token"

instance FromJSON UserDetails where
    parseJSON = withObject "UserDetails" $ \v ->
        UserDetails
            <$> v .: keyOauthToken
            <*> v .: keyOauthTokenSecret
            <*> v .: keyUserId
            <*> v .: keyScreenName
            <*> v .: keyAccessRequestToken

instance ToJSON UserDetails where
    -- this generates a Value
    toJSON user =
        object
            [ keyOauthToken .= oauthToken user
            , keyOauthTokenSecret .= oauthTokenSecret user
            , keyUserId .= userId user
            , keyScreenName .= screenName user
            , keyAccessRequestToken .= accessRequestToken user
            ]

toBSON :: UserDetails -> Bson.Document
toBSON user =
    let s = String . T.pack
     in [ keyOauthToken := s (oauthToken user)
        , keyOauthTokenSecret := s (oauthTokenSecret user)
        , keyUserId := s (userId user)
        , keyScreenName := s (screenName user)
        , keyAccessRequestToken := s (accessRequestToken user)
        ]

fromBSON :: Bson.Document -> Either String UserDetails
fromBSON doc =
    let getProp name =
            maybe
                (Left $ "No " ++ T.unpack name ++ " field in credential.")
                Right
                (Bson.lookup name doc :: Maybe String)
     in do
            token <- getProp keyOauthToken
            tokenSecret <- getProp keyOauthTokenSecret
            uId <- getProp keyUserId
            sName <- getProp keyScreenName
            aRToken <- getProp keyAccessRequestToken
            return
                UserDetails
                    { oauthToken = token
                    , oauthTokenSecret = tokenSecret
                    , userId = uId
                    , screenName = sName
                    , accessRequestToken = aRToken
                    }
