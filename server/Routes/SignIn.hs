{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Routes.SignIn (get) where

import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant (Handler, err401, throwError)
import Types (DBActionRunner, InfoMsg (InfoMsg))
import Web.Authenticate.OAuth as OAuth

get ::  OAuth.OAuth -> DBActionRunner -> Manager -> Maybe String -> Handler InfoMsg
get oauth _ manager mAppSessionId =
    liftIO $ do
        let
            msg = "app_session_id = " ++ (show mAppSessionId)
        credentials <- OAuth.getTemporaryCredential oauth manager
        print $ "Credentials found" ++ show credentials
        print msg
        return $ InfoMsg msg
