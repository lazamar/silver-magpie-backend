{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Routes.AppGetAccess (get, ReturnType) where
import Authenticate (authenticate)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Object, decode, (.:))
import Data.Aeson.Types (Parser, ToJSON, parseMaybe)
import qualified Data.ByteString.Lazy.Char8 as LByteString
import GHC.Generics (Generic)
import MongoTypes.UserDetails (UserDetails)
import qualified MongoTypes.UserDetails as UserDetails
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

data ReturnType =
     ReturnType
         { app_access_token        :: String
         , screen_name             :: String
         , profile_image_url_https :: String
         }
         deriving (Show, Generic)

instance ToJSON ReturnType


get :: OAuth.OAuth -> Manager -> DBActionRunner -> Maybe String -> Handler ReturnType
get _ _ _ Nothing = throwError err401
get oauth manager runDbAction (Just sessionId) =
    do
        mUser <- liftIO $ runDbAction $ authenticate sessionId
        case mUser of
            Nothing ->
                throwError err401
            Just userDetails ->
                do
                    response <- liftIO $ mainUserDetails oauth manager userDetails
                    case response of
                        Left err ->
                            throwError $ err500 { errBody = LByteString.pack err }
                        Right profileImage ->
                            return
                                ReturnType
                                    { app_access_token = sessionId
                                    , screen_name = UserDetails.screenName userDetails
                                    , profile_image_url_https = profileImage
                                    }


mainUserDetails :: OAuth.OAuth -> Manager -> UserDetails -> IO (Either String String)
mainUserDetails oauth manager userDetails =
    do
        eitherRequest <- queryApi requestConfig
        return $ eitherRequest >>= toTwitterDetails . responseBody
    where
        requestConfig =
            RequestConfig
                { configOauth = oauth
                , configManager = manager
                , configUserDetails = userDetails
                , configUrl = "https://api.twitter.com/1.1/account/verify_credentials.json"
                , configMethod = "GET"
                , configQuery = []
                }

        toTwitterDetails json =
            maybe (Left "Failed to decode twitter response") Right $
            parseProfileUrl =<<
            (decode json :: Maybe Object)

        parseProfileUrl :: Object -> Maybe String
        parseProfileUrl =
            parseMaybe $ \obj ->
                obj .: "profile_image_url_https" :: Parser String
