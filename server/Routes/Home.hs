{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Routes.Home (get, ReturnType, tweets) where

import Authenticate (authenticate)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, decode)
import Data.Aeson.Types (ToJSON)
import qualified Data.ByteString.Lazy.Char8 as LB
import GHC.Generics (Generic)
import MongoTypes.UserDetails (UserDetails)
import Network.HTTP.Client (Manager, responseBody)
import Servant (Handler, err401, err500, errBody, throwError)
import Twitter
    ( RequestConfig (RequestConfig)
    , configManager
    , configMethod
    , configOauth
    , configQuery
    , configUrl
    , configUserDetails
    , queryApi
    )
import Types (DBActionRunner)
import qualified Web.Authenticate.OAuth as OAuth

newtype ReturnType =
    ReturnType
        { tweets :: Value
        }
        deriving (Show, Generic)

instance ToJSON ReturnType


get ::
    OAuth.OAuth
    -> Manager
    -> DBActionRunner
    -> Maybe String
    -> Maybe String
    -> Maybe String
    -> Handler ReturnType
get _ _ _ Nothing _ _ = throwError err401
get oauth manager runDbAction (Just sessionId) mMaxId mSinceId =
    do
        mUser <- liftIO $ runDbAction $ authenticate sessionId
        case mUser of
            Nothing ->
                throwError err401
            Just userDetails ->
                do
                    eTweets <- liftIO $ fetchTweets oauth manager userDetails mMaxId mSinceId
                    case eTweets of
                        Left err ->
                            throwError $ err500 { errBody = LB.pack err }
                        Right r ->
                            return r

fetchTweets :: OAuth.OAuth -> Manager -> UserDetails -> Maybe String -> Maybe String -> IO (Either String ReturnType)
fetchTweets oauth manager userDetails mSinceId mMaxId =
    do
        eitherRequest <- queryApi requestConfig
        return $ eitherRequest >>= toTweets . responseBody
    where
        requestConfig =
            RequestConfig
                { configOauth = oauth
                , configManager = manager
                , configUserDetails = userDetails
                , configUrl = "https://api.twitter.com/1.1/statuses/home_timeline.json"
                , configMethod = "GET"
                , configQuery =
                    [ ("max_id", mMaxId >>= removeEmpty)
                    , ("since_id", mSinceId >>= removeEmpty)
                    ]
                }

        toTweets json =
            maybe
                (Left "Failed to decode twitter response")
                (Right . ReturnType)
                (decode json :: Maybe Value)

        removeEmpty v =
            if null v then
                Nothing
            else
                Just v
