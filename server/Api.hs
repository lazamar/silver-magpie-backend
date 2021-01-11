{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Api (Api, apiServer) where

import Authenticate (Authenticate)
import Control.Monad.Database (MonadDB)
import Data.Aeson (Value)
import Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import MongoTypes.AppAuth (AppAuth)
import MongoTypes.UserDetails (UserDetails)
import Network.HTTP.Client (Manager)
import Servant
    ( Delete,
      Get,
      Header,
      JSON,
      Post,
      QueryParam,
      ReqBody,
      Server,
      ServerT,
      (:<|>) ((:<|>)),
      (:>),
    )
import Twitter (Timeline)
import Types
    ( DBActionRunner,
      EnvironmentVariables,
      HandlerM,
      InfoMsg,
      domain,
      twitterKey,
      twitterSecret,
    )
import Web.Authenticate.OAuth as OAuth
import qualified Control.Monad.Database as DB
import qualified Routes.AppGetAccess
import qualified Routes.AppRevokeAccess
import qualified Routes.Favorite
import qualified Routes.Home
import qualified Routes.Mentions
import qualified Routes.Retweet
import qualified Routes.SaveCredentials
import qualified Routes.SignIn
import qualified Routes.StatusUpdate
import qualified Routes.UserSearch

-------------------------------------------------------------------------------
--                               API
-------------------------------------------------------------------------------

type Api =
    "sign-in"
        :> QueryParam "app_session_id" String
        :> Get '[JSON] AppAuth
        :<|> "save-credentials"
            :> QueryParam "oauth_token" String
            :> QueryParam "oauth_verifier" String
            :> Get '[JSON] InfoMsg
        :<|> "app-get-access"
            :> Authenticate
            :> Header "x-app-token" String
            :> Get '[JSON] Routes.AppGetAccess.ReturnType
        :<|> "app-revoke-access"
            :> Authenticate
            :> Delete '[JSON] InfoMsg
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
        :<|> "status-update"
            :> Authenticate
            :> ReqBody '[JSON] Routes.StatusUpdate.StatusBody
            :> Post '[JSON] Value
        :<|> "favorite"
            :> Authenticate
            :> QueryParam "id" String
            :> Post '[JSON] Value
        :<|> "favorite"
            :> Authenticate
            :> QueryParam "id" String
            :> Delete '[JSON] Value
        :<|> "retweet"
            :> Authenticate
            :> QueryParam "id" String
            :> Post '[JSON] Value
        :<|> "retweet"
            :> Authenticate
            :> QueryParam "id" String
            :> Delete '[JSON] Value

-------------------------------------------------------------------------------
--                               Handlers
-------------------------------------------------------------------------------

type M m =
    ( HandlerM m
    , DB.ToRecord m AppAuth
    , DB.ToRecord m UserDetails
    )

apiServer :: M m => EnvironmentVariables -> DBActionRunner m -> Manager -> ServerT Api m
apiServer env runDbAction manager =
    let oauth = twitterOAuth env
     in Routes.SignIn.get oauth manager
            :<|> Routes.SaveCredentials.get oauth manager
            :<|> Routes.AppGetAccess.get oauth manager
            :<|> Routes.AppRevokeAccess.delete runDbAction
            :<|> Routes.Home.get oauth manager
            :<|> Routes.Mentions.get oauth manager
            :<|> Routes.UserSearch.get oauth manager
            :<|> Routes.StatusUpdate.post oauth manager
            :<|> Routes.Favorite.post oauth manager
            :<|> Routes.Favorite.delete oauth manager
            :<|> Routes.Retweet.post oauth manager
            :<|> Routes.Retweet.delete oauth manager

twitterOAuth :: EnvironmentVariables -> OAuth.OAuth
twitterOAuth env =
    OAuth.newOAuth
        { oauthServerName = "https://api.twitter.com/"
        , oauthRequestUri = "https://api.twitter.com/oauth/request_token"
        , oauthAccessTokenUri = "https://api.twitter.com/oauth/access_token"
        , oauthAuthorizeUri = "https://api.twitter.com/oauth/authorize"
        , oauthConsumerKey = encodeUtf8 $ twitterKey env
        , oauthConsumerSecret = encodeUtf8 $ twitterSecret env
        , oauthCallback = Just $ encodeUtf8 $ T.append (domain env) "/save-credentials"
        }
