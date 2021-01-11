{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Routes.SignIn (get) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bson ((=:))
import MongoTypes.AppAuth (AppAuth (AppAuth , accessRequestToken , appSessionId))
import Network.HTTP.Client (Manager)
import Network.HTTP.Types.Header (hLocation)
import Servant (Handler, ServerError, err301, err400, err500, errBody, errHeaders, throwError)
import Types (HandlerM, targetCollection)
import Web.Authenticate.OAuth as OAuth
import qualified Control.Monad.Database as DB
import qualified Data.ByteString.Char8 as ByteString
import qualified MongoTypes.AppAuth as AppAuth

type M m =
    ( HandlerM m
    , DB.ToRecord m AppAuth
    )

get :: M m => OAuth.OAuth -> Manager -> Maybe String -> m AppAuth
get oauth manager mAppSessionId =
    let authorise Nothing _ =
            throwError $
                err400
                    { errBody = "No app_session_id provided"
                    }
        authorise _ Nothing =
            throwError $
                err500
                    { errBody = "Something unexpected happened.No OAuth token provided by API server."
                    }
        authorise (Just sessionId) (Just token) =
            let auth = AppAuth sessionId token
             in saveAppAuthorisation auth >> authoriseWithToken token
     in do
            credentials <- liftIO $ OAuth.getTemporaryCredential oauth manager
            authorise mAppSessionId $ oauthToken credentials

oauthToken :: Credential -> Maybe String
oauthToken credential =
    fmap
        (removeQuotes . show)
        $ lookup "oauth_token" $
            unCredential credential

removeQuotes :: [a] -> [a]
removeQuotes v =
    drop 1 $ take (length v - 1) v

saveAppAuthorisation :: M m => AppAuth -> m ()
saveAppAuthorisation auth =
    let document = AppAuth (appSessionId auth) (accessRequestToken auth)
        selector = ["app_session_id" DB.=: appSessionId auth]
        collection = "app-authorisation"
     in DB.store
            (targetCollection AppAuth.collectionName)
            (DB.Update selector document)

authoriseWithToken :: MonadError ServerError m => String -> m a
authoriseWithToken token =
    let url = "https://api.twitter.com/oauth/authorize?oauth_token=" ++ token
        locationHeader = (hLocation , ByteString.pack url)
        otherHeaders = errHeaders err301
        err =
            err301
                { errHeaders = locationHeader : otherHeaders
                }
     in throwError err
