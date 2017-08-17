{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Routes.AppGetAccess (get, ReturnType) where
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (eitherDecode)
import Data.Aeson.Types (FromJSON, ToJSON, parseJSON, withObject, (.:))
import Data.Bson (Document, (=:))
import qualified Data.Bson as Bson
import Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy.Char8 as LByteString
import Database.MongoDB.Query (Action, findOne)
import qualified Database.MongoDB.Query as Mongo
import GHC.Generics (Generic)
import MongoTypes.UserDetails (UserDetails, oauthToken, oauthTokenSecret)
import qualified MongoTypes.UserDetails as UserDetails
import Network.HTTP.Client (Manager, httpLbs, parseRequest, responseBody)
import Servant (Handler, err401, err500, errBody, throwError)
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
        mUser <- liftIO $ runDbAction $ getUserDetailsFromSessionId sessionId
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

newtype ProfileImage =
    ProfileImage
        { profileImageUrlHttps :: String
        } deriving (Show, Generic)

instance FromJSON ProfileImage where
    parseJSON = withObject "ProfileImage" $ \v -> ProfileImage
        <$> v .: "profile_image_url_https"


mainUserDetails :: OAuth.OAuth -> Manager -> UserDetails -> IO (Either String String)
mainUserDetails oauth manager userDetails =
    do
        r1 <- parseRequest $ "GET " ++ endpoint
        r2 <- OAuth.signOAuth oauth credentials r1
        response <- httpLbs r2 manager
        return $ toTwitterDetails $ responseBody response
    where
        endpoint =
            "https://api.twitter.com/1.1/account/verify_credentials.json"

        credentials =
            OAuth.insert "oauth_token_secret" (ByteString.pack $ oauthTokenSecret userDetails)
            $ OAuth.insert "oauth_token" (ByteString.pack $ oauthToken userDetails)
            OAuth.emptyCredential

        toTwitterDetails json =
            profileImageUrlHttps
            <$> (eitherDecode json :: Either String ProfileImage)

getRequestTokenBySessionId :: MonadIO m => String -> Action m (Maybe Document)
getRequestTokenBySessionId sessionId =
    let
        selector = [ "app_session_id" =: sessionId ]
        collection = "app-authorisation"
        selection = Mongo.select selector collection
    in
        findOne selection

getUserDetailsByRequestToken :: MonadIO m => String -> Action m (Maybe UserDetails)
getUserDetailsByRequestToken token =
    let
        selector = [ "access_request_token" =: token ]
        collection = "credentials"
        selection = Mongo.select selector collection
    in
        (either (const Nothing) Just
        . maybe (Left "Token not found") UserDetails.fromBSON
        )
        <$> findOne selection

getUserDetailsFromSessionId :: MonadIO m => String -> Action m (Maybe UserDetails)
getUserDetailsFromSessionId sessionId =
    do
        mToken <- (accessRequestToken =<<) <$> getRequestTokenBySessionId sessionId
        case mToken of
            Nothing ->
                return Nothing
            Just token ->
                getUserDetailsByRequestToken token
    where
        accessRequestToken doc =
            Bson.lookup "access_request_token" doc ::Maybe String
