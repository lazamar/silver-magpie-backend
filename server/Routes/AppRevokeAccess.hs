{-# LANGUAGE RankNTypes #-}

module Routes.AppRevokeAccess (delete) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bson ((=:))
import Data.Text as T
import Database.MongoDB.Query (Action, Selection (Select))
import qualified Database.MongoDB.Query as Mongo
import qualified MongoTypes.AppAuth as AppAuth
import MongoTypes.UserDetails (UserDetails)
import qualified MongoTypes.UserDetails as UserDetails
import Servant (Handler)
import Types (DBActionRunner, InfoMsg (InfoMsg))

delete :: DBActionRunner -> UserDetails -> Handler InfoMsg
delete runDbAction userDetails =
    do
        _ <- liftIO $
            runDbAction $
            deleteByRequestToken $
            UserDetails.accessRequestToken userDetails
        return $ InfoMsg "Authorisation revoked."


{- Delete records in both collections -}
deleteByRequestToken :: MonadIO m => String -> Action m ()
deleteByRequestToken token =
    let
        userDetailsSelector =
            Select
                [ UserDetails.keyAccessRequestToken =: T.pack token ]
                UserDetails.collectionName
        appAuthSelection =
            Select
                [ AppAuth.keyAccessRequestToken =: T.pack  token ]
                AppAuth.collectionName
    in
        Mongo.delete userDetailsSelector >>
        Mongo.delete appAuthSelection
