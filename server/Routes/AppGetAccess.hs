{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Routes.AppGetAccess (get) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bson (Document, (=:))
import Database.MongoDB.Query (Action, findOne)
import qualified Database.MongoDB.Query as Mongo
import Servant (Handler, err401, throwError)
import Types (DBActionRunner, InfoMsg (InfoMsg))
import qualified Web.Authenticate.OAuth as OAuth

get :: OAuth.OAuth -> DBActionRunner -> Maybe String -> Handler InfoMsg
get _ _ Nothing = throwError err401
get _ runDbAction (Just appToken) =
    do
        mCredentials <- liftIO $ runDbAction $ getCredentialsByAppToken appToken
        case mCredentials of
            Nothing ->
                throwError err401

            Just credentials ->
                liftIO (print $ show credentials)
                >> (return $ InfoMsg "Authorised")


getCredentialsByAppToken :: MonadIO m => String -> Action m (Maybe Document)
getCredentialsByAppToken appToken =
    let
        selector = [ "app_session_id" =: appToken ]
        collection = "app-authorisation"
        selection = Mongo.select selector collection
    in
        findOne selection
