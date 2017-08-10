{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Api (Api, apiServer) where

import Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import MongoTypes.AppAuth (AppAuth)
import Network.HTTP.Client (Manager)
import Routes.AppGetAccess
import Routes.SaveCredentials
import Routes.SignIn
import Servant ((:<|>) ((:<|>)), (:>), Get, JSON, QueryParam, Server)
import Types
    ( DBActionRunner
    , EnvironmentVariables
    , InfoMsg
    , domain
    , twitterKey
    , twitterSecret
    )
import Web.Authenticate.OAuth as OAuth
-------------------------------------------------------------------------------
--                               API
-------------------------------------------------------------------------------

type Api =  "sign-in" :> QueryParam "app_session_id" String :> Get '[JSON] AppAuth
    :<|>    "save-credentials" :> QueryParam "oauth_token" String :> QueryParam "oauth_verifier" String :> Get '[JSON] InfoMsg
    :<|>    "app-get-access" :> Get '[JSON] InfoMsg

-------------------------------------------------------------------------------
--                               Handlers
-------------------------------------------------------------------------------

apiServer :: EnvironmentVariables -> DBActionRunner ->  Manager -> Server Api
apiServer env runDbAction manager =
    let
        oauth = twitterOAuth env
    in
            Routes.SignIn.get oauth runDbAction manager
    :<|>    Routes.SaveCredentials.get oauth runDbAction manager
    :<|>    Routes.AppGetAccess.get


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
