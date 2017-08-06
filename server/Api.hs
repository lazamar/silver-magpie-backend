{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Api (Api, apiServer) where

import Control.Monad.IO.Class (liftIO)
import Data.Bson (Document)
import Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import Database.MongoDB (Action, rest, select)
import qualified Database.MongoDB.Query as Query
import Network.HTTP.Client.TLS (newTlsManager)
import Servant
    ( (:<|>) ((:<|>))
    , (:>)
    , Get
    , Handler
    , JSON
    , QueryParam
    , Server
    , err401
    , throwError
    )
import Types
    ( DBActionRunner
    , EnvironmentVariables
    , InfoMsg (InfoMsg)
    , domain
    , twitterKey
    , twitterSecret
    )
import Web.Authenticate.OAuth as OAuth

-------------------------------------------------------------------------------
--                               API
-------------------------------------------------------------------------------

type Api =  "sign-in" :> QueryParam "app_session_id" String :> Get '[JSON] InfoMsg
    :<|>    "save-credentials" :> QueryParam "oauth_token" String :> Get '[JSON] InfoMsg
    :<|>    "app-get-access" :> Get '[JSON] InfoMsg
    :<|>    "test2" :> Get '[JSON] InfoMsg

-------------------------------------------------------------------------------
--                               Handlers
-------------------------------------------------------------------------------

apiServer :: EnvironmentVariables -> DBActionRunner -> Server Api
apiServer env runDbAction =
            signIn env runDbAction
    :<|>    saveCreadentials env runDbAction
    :<|>    appGetAccess
    :<|>    test2 runDbAction


getFindOne :: Action IO (Maybe Document)
getFindOne = Query.findOne $ select [] "credentials"


getCursor :: Action IO Query.Cursor
getCursor = Query.find $ select [] "credentials"


signIn ::  EnvironmentVariables -> DBActionRunner -> Maybe String -> Handler InfoMsg
signIn env _ mAppSessionId =
    liftIO $ do
        let
            oAuth = twitterOAuth env
            msg = "app_session_id = " ++ show mAppSessionId
        manager <- newTlsManager
        credentials <- OAuth.getTemporaryCredential oAuth manager
        print $ "Credentials found" ++ show credentials
        print msg
        return $ InfoMsg msg


saveCreadentials :: EnvironmentVariables -> DBActionRunner -> Maybe String -> Handler InfoMsg
saveCreadentials env _ mOAuthAccessToken =
    liftIO $ do
        print $ show $ twitterOAuth env
        print $ T.unpack "save-credentials called!!!"
        return $ InfoMsg  (show mOAuthAccessToken)

appGetAccess :: Handler InfoMsg
appGetAccess =
    throwError err401

test2 :: DBActionRunner -> Handler InfoMsg
test2 runDbAction = liftIO $ do
    theOne <- runDbAction getFindOne
    print $ show theOne
    cursor <- runDbAction getCursor
    vals <- runDbAction $ rest cursor
    return $ InfoMsg  (show vals)


twitterOAuth :: EnvironmentVariables -> OAuth.OAuth
twitterOAuth env =
    OAuth.newOAuth
        { oauthServerName = "https://api.twitter.com/"
        , oauthRequestUri = "https://api.twitter.com/oauth/request_token"
        , oauthAccessTokenUri = "https://api.twitter.com/oauth/access_token"
        , oauthAuthorizeUri  = "https://api.twitter.com/oauth/authorize"
        , oauthConsumerKey = encodeUtf8 $ twitterKey env
        , oauthConsumerSecret = encodeUtf8 $ twitterSecret env
        , oauthCallback = Just $ encodeUtf8 $ T.append (domain env) "/save-credentials"
        }
