{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api (Api, apiServer) where

import Authenticate (Authenticate)
import Data.Aeson (Value)
import Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import MongoTypes.AppAuth (AppAuth)
import Network.HTTP.Client (Manager)
import qualified Routes.AppGetAccess
import qualified Routes.Home
import qualified Routes.Mentions
import qualified Routes.SaveCredentials
import qualified Routes.SignIn
import qualified Routes.UserSearch
import Servant ((:<|>) ((:<|>)), (:>), Get, JSON, QueryParam, Server)
import Twitter (Timeline)
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

type Api =
    "sign-in"
            :> QueryParam "app_session_id" String
            :> Get '[JSON] AppAuth
    :<|> "save-credentials"
            :> QueryParam "oauth_token" String
            :> QueryParam "oauth_verifier" String :> Get '[JSON] InfoMsg
    :<|> "app-get-access"
            :> Authenticate
            :> Get '[JSON] Routes.AppGetAccess.ReturnType
    :<|> "home"
            :> Authenticate
            :> QueryParam "sinceId" String
            :> QueryParam "maxId" String
            :> Get '[JSON] Timeline
    :<|> "mentions"
            :> Authenticate
            :> QueryParam "sinceId" String
            :> QueryParam "maxId" String
            :> Get '[JSON] Timeline
    :<|> "user-search"
            :> Authenticate
            :> QueryParam "q" String
            :> Get '[JSON] Value

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
    :<|>    Routes.AppGetAccess.get oauth manager
    :<|>    Routes.Home.get oauth manager
    :<|>    Routes.Mentions.get oauth manager
    :<|>    Routes.UserSearch.get oauth manager


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
