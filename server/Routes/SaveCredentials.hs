{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Routes.SaveCredentials (get) where

import Control.Monad.IO.Class (liftIO)
import Data.Text as T
import Network.HTTP.Client.TLS (newTlsManager)
import Servant (Handler, err401, throwError)
import Types (DBActionRunner, InfoMsg (InfoMsg))
import Web.Authenticate.OAuth as OAuth

get :: OAuth.OAuth -> DBActionRunner -> Maybe String -> Handler InfoMsg
get oauth _ mOAuthAccessToken =
    liftIO $ do
        print $ show oauth
        print $ T.unpack "save-credentials called!!!"
        return $ InfoMsg  (show mOAuthAccessToken)
