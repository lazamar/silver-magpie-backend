{-# LANGUAGE RankNTypes #-}

module Authenticate (authenticate) where


import Control.Monad.IO.Class (MonadIO)
import Data.Bson (Document, (=:))
import Data.Either.Combinators (rightToMaybe)
import Database.MongoDB.Query (Action, Collection, Selector, findOne, select)
import MongoTypes.AppAuth (AppAuth)
import qualified MongoTypes.AppAuth as AppAuth
import MongoTypes.UserDetails (UserDetails)
import qualified MongoTypes.UserDetails as UserDetails


authenticate :: MonadIO m => String -> Action m (Maybe UserDetails)
authenticate sessionId =
    do
        mAppAuth <- getAppAuthBySessionId sessionId
        case mAppAuth of
            Nothing ->
                return Nothing
            Just appAuth ->
                getUserDetailsByRequestToken $ AppAuth.accessRequestToken appAuth


getAppAuthBySessionId :: MonadIO m => String -> Action m (Maybe AppAuth)
getAppAuthBySessionId sessionId =
    fetch
        [ AppAuth.keyAppSessionId =: sessionId ]
        AppAuth.collectionName
        AppAuth.fromBSON


getUserDetailsByRequestToken :: MonadIO m => String -> Action m (Maybe UserDetails)
getUserDetailsByRequestToken token =
    fetch
        [ UserDetails.keyAccessRequestToken =: token ]
        UserDetails.collectionName
        UserDetails.fromBSON


fetch :: MonadIO m => Selector -> Collection -> (Document -> Either e a) -> Action m (Maybe a)
fetch selector collection decoder =
    (=<<) (rightToMaybe . decoder) <$> findOne selection
    where
        selection =
            select selector collection
