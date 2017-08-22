{-# LANGUAGE DeriveGeneric #-}

module Twitter.Timeline (Timeline, fetchHome, fetchMentions, tweets) where

import Data.Aeson (Value, decode)
import Data.Aeson.Types (ToJSON)
import GHC.Generics (Generic)
import MongoTypes.UserDetails (UserDetails)
import Network.HTTP.Client (Manager, responseBody)
import Twitter.Query
    ( RequestConfig (RequestConfig)
    , configManager
    , configMethod
    , configOauth
    , configQuery
    , configUrl
    , configUserDetails
    , queryApi
    )
import qualified Web.Authenticate.OAuth as OAuth

newtype Timeline =
    Timeline
        { tweets :: Value
        }
        deriving (Show, Generic)

instance ToJSON Timeline


fetchHome :: OAuth.OAuth -> Manager -> UserDetails -> Maybe String -> Maybe String -> IO (Either String Timeline)
fetchHome =
    fetchTweets "home_timeline"

fetchMentions :: OAuth.OAuth -> Manager -> UserDetails -> Maybe String -> Maybe String -> IO (Either String Timeline)
fetchMentions =
    fetchTweets "mentions_timeline"


fetchTweets :: String -> OAuth.OAuth -> Manager -> UserDetails -> Maybe String -> Maybe String -> IO (Either String Timeline)
fetchTweets timeline oauth manager userDetails mSinceId mMaxId =
    do
        eitherRequest <- queryApi requestConfig
        return $ eitherRequest >>= toTweets . responseBody
    where
        requestConfig =
            RequestConfig
                { configOauth = oauth
                , configManager = manager
                , configUserDetails = userDetails
                , configUrl = "https://api.twitter.com/1.1/statuses/" ++ timeline ++ ".json"
                , configMethod = "GET"
                , configQuery =
                    [ ("max_id", mMaxId >>= removeEmpty)
                    , ("since_id", mSinceId >>= removeEmpty)
                    ]
                }

        toTweets json =
            maybe
                (Left "Failed to decode twitter response")
                (Right . Timeline)
                (decode json :: Maybe Value)

        removeEmpty v =
            if null v then
                Nothing
            else
                Just v
