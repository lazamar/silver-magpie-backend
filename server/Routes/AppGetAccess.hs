{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Routes.AppGetAccess (get, ReturnType) where
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Object, decode, (.:))
import Data.Aeson.Types (Parser, ToJSON, parseMaybe)
import qualified Data.ByteString.Lazy.Char8 as LByteString
import GHC.Generics (Generic)
import MongoTypes.UserDetails (UserDetails)
import qualified MongoTypes.UserDetails as UserDetails
import Network.HTTP.Client (Manager, responseBody)
import Servant (err401, err500, errBody, throwError)
import qualified Twitter
import qualified Web.Authenticate.OAuth as OAuth
import Types (HandlerM)

data ReturnType =
     ReturnType
         { app_access_token        :: String
         , screen_name             :: String
         , profile_image_url_https :: String
         }
         deriving (Show, Generic)

instance ToJSON ReturnType


get :: HandlerM m => OAuth.OAuth -> Manager -> UserDetails -> Maybe String -> m ReturnType
get _ _ _ Nothing  = throwError err401
get oauth manager userDetails (Just sessionId) =
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
        eitherRequest <- fetch
        return $ eitherRequest >>= toTwitterDetails . responseBody
    where
        fetch =
            Twitter.queryApi
                "GET"
                "https://api.twitter.com/1.1/account/verify_credentials.json"
                []
                oauth
                manager
                userDetails

        toTwitterDetails json =
            maybe (Left "Failed to decode twitter response") Right $
            parseProfileUrl =<<
            (decode json :: Maybe Object)

        parseProfileUrl :: Object -> Maybe String
        parseProfileUrl =
            parseMaybe $ \obj ->
                obj .: "profile_image_url_https" :: Parser String
