{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Routes.SignIn (get) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bson ((=:))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.Text.Encoding (decodeUtf8)
import Database.MongoDB.Query (Action, Selection (Select), upsert)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Header (hLocation)
import Servant
    ( Handler
    , ServantErr
    , err301
    , err400
    , err500
    , errBody
    , errHeaders
    , throwError
    )
import Types
    ( AppAuth (AppAuth, accessRequestToken, appSessionId)
    , DBActionRunner
    , InfoMsg (InfoMsg)
    )
import Web.Authenticate.OAuth as OAuth

get ::  OAuth.OAuth -> DBActionRunner -> Manager -> Maybe String -> Handler AppAuth
get oauth runDbAction manager mAppSessionId =
    case mAppSessionId of
        Nothing ->
            throwError $ err400
                { errBody = "No app_session_id provided" }

        Just sId ->
            do
                credentials <- liftIO $ OAuth.getTemporaryCredential oauth manager
                case oauthToken credentials of
                    Nothing ->
                        throwError $ err500
                            { errBody = "Something unexpected happened.No OAuth token provided by API server." }

                    Just token ->
                        let
                            auth = AppAuth sId token
                            action = saveAppAuthorisation auth
                        in
                            liftIO (runDbAction action)
                            >> authoriseWithToken token


oauthToken :: Credential -> Maybe String
oauthToken credential =
    fmap
        (removeQuotes . show)
        $ lookup "oauth_token"
        $ unCredential credential


removeQuotes :: [a] -> [a]
removeQuotes v =
    drop 1 $ take (length v - 1) v


saveAppAuthorisation :: MonadIO m => AppAuth -> Action m ()
saveAppAuthorisation auth =
    let
        document =
            [ "app_session_id" =: appSessionId auth
            , "access_request_token" =: accessRequestToken auth
            ]
        selector = [ "app_session_id" =: appSessionId auth ]
        collection = "app-authorisation"
        selection = Select selector collection
    in
        upsert selection document

authoriseWithToken :: String -> Handler a
authoriseWithToken token =
    let
        url = "https://api.twitter.com/oauth/authorize?oauth_token=" ++ token
        locationHeader = (hLocation, ByteString.pack url)
        otherHeaders = errHeaders err301
        err = err301
            { errHeaders = locationHeader:otherHeaders }
    in
        throwError err
