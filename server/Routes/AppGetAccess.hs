{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Routes.AppGetAccess (get) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bson (Document, (=:))
import qualified Data.Bson as Bson
import Database.MongoDB.Query (Action, findOne)
import qualified Database.MongoDB.Query as Mongo
import MongoTypes.UserDetails (UserDetails)
import qualified MongoTypes.UserDetails as UserDetails
import Servant (Handler, err401, throwError)
import Types (DBActionRunner, InfoMsg (InfoMsg))
import qualified Web.Authenticate.OAuth as OAuth

get :: OAuth.OAuth -> DBActionRunner -> Maybe String -> Handler InfoMsg
get _ _ Nothing = throwError err401
get _ runDbAction (Just sessionId) =
    do
        mCredentials <- liftIO $ runDbAction $ getRequestTokenBySessionId sessionId
        let
            mToken = mCredentials >>= (Bson.lookup "access_request_token" :: Document -> Maybe String)
        case mToken of
            Nothing ->
                throwError err401

            Just token ->
                do
                    mUser <- liftIO $ runDbAction $ getUserDetailsByRequestToken token

                    case mUser of
                        Nothing ->
                            throwError err401
                        Just user ->
                            liftIO (print $ show user)
                            >> (return $ InfoMsg "Authorised")



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
