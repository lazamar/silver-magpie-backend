{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Routes.AppGetAccess (get) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bson (Document, (=:))
import qualified Data.Bson as Bson
import Data.ByteString.Char8 as ByteString
import Database.MongoDB.Query (Action, findOne)
import qualified Database.MongoDB.Query as Mongo
import MongoTypes.UserDetails (UserDetails, oauthToken, oauthTokenSecret)
import qualified MongoTypes.UserDetails as UserDetails
import Network.HTTP.Client (Manager, httpLbs, parseRequest, responseBody)
import Servant (Handler, err401, throwError)
import Types (DBActionRunner, InfoMsg (InfoMsg))
import qualified Web.Authenticate.OAuth as OAuth


get :: OAuth.OAuth -> Manager -> DBActionRunner -> Maybe String -> Handler InfoMsg
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
                    return $ InfoMsg response



mainUserDetails :: OAuth.OAuth -> Manager -> UserDetails -> IO String
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

        toTwitterDetails =
            show

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
