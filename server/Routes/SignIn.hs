{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Routes.SignIn (get) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bson ((=:))
import Database.MongoDB.Query (Action, Selection (Select), upsert)
import MongoTypes.AppAuth (AppAuth (AppAuth , accessRequestToken , appSessionId))
import Network.HTTP.Client (Manager)
import Network.HTTP.Types.Header (hLocation)
import Servant (Handler, ServerError, err301, err400, err500, errBody, errHeaders, throwError)
import Types (DBActionRunner, HandlerM)
import Web.Authenticate.OAuth as OAuth
import qualified Data.ByteString.Char8 as ByteString

get :: HandlerM m => OAuth.OAuth -> DBActionRunner m -> Manager -> Maybe String -> m AppAuth
get oauth runDbAction manager mAppSessionId =
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
                action = saveAppAuthorisation auth
             in runDbAction action >> authoriseWithToken token
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

saveAppAuthorisation :: MonadIO m => AppAuth -> Action m ()
saveAppAuthorisation auth =
    let document =
            [ "app_session_id" =: appSessionId auth
            , "access_request_token" =: accessRequestToken auth
            ]
        selector = ["app_session_id" =: appSessionId auth]
        collection = "app-authorisation"
        selection = Select selector collection
     in upsert selection document

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
