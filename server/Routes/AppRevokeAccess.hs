{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Routes.AppRevokeAccess (delete) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text as T
import Types.UserDetails (UserDetails)
import Servant (Handler)
import Types (HandlerM, InfoMsg (InfoMsg), targetCollection)
import qualified Control.Monad.Database as DB
import qualified Types.AppAuth as AppAuth
import qualified Types.UserDetails as UserDetails

delete :: HandlerM m => UserDetails -> m InfoMsg
delete userDetails = do
    deleteByRequestToken $ UserDetails.accessRequestToken userDetails
    return $ InfoMsg "Authorisation revoked."

{- Delete records in both collections -}
deleteByRequestToken :: HandlerM m => String -> m ()
deleteByRequestToken token =
    let userDetailsSelector =
            DB.DeleteMany [UserDetails.keyAccessRequestToken DB.=: T.pack token]
        appAuthSelection =
            DB.DeleteMany [AppAuth.keyAccessRequestToken DB.=: T.pack token]
     in DB.delete (targetCollection UserDetails.collectionName) userDetailsSelector
            >> DB.delete (targetCollection UserDetails.collectionName) appAuthSelection
