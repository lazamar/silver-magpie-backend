{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Routes.SaveCredentials (get) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Bson as Bson
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy.Char8 as LByteString
import qualified Data.Text as T
import Database.MongoDB.Query (Action)
import qualified Database.MongoDB.Query as Mongo
import MongoTypes.UserDetails
    ( UserDetails (UserDetails)
    , accessRequestToken
    , oauthToken
    , oauthTokenSecret
    , screenName
    , toBSON
    , userId
    )
import Network.HTTP.Client (Manager)
import Network.HTTP.Types.Header (hLocation)
import Servant
    (Handler, err301, err400, err500, errBody, errHeaders, throwError)

import Types (DBActionRunner, InfoMsg)
import Web.Authenticate.OAuth as OAuth

get :: OAuth.OAuth -> DBActionRunner -> Manager -> Maybe String -> Maybe String -> Handler InfoMsg
get _ _ _ Nothing _ = throwError err400
get _ _ _ _ Nothing = throwError err400
get oauth runDBAction manager (Just requestToken) (Just requestVerifier) =
        let
            reqCredentials =
                    OAuth.injectVerifier (ByteString.pack requestVerifier)
                    $ OAuth.newCredential (ByteString.pack requestToken) ""
        in
        do
            credentials <- liftIO $ OAuth.getAccessToken oauth reqCredentials manager
            let
                eUserDetails = toUserDetails requestToken credentials

            case eUserDetails of
                Left err ->
                    throwError $ err500 { errBody = LByteString.pack err }

                Right userDetails ->
                    liftIO (runDBAction $ saveUserDetails userDetails)
                    >> liftIO (print $ T.unpack "User Details saved for " ++ screenName userDetails)
                    >> redirectTo "./thank-you.html"



toUserDetails :: String -> Credential -> Either String UserDetails
toUserDetails requestToken credentials =
    let
        dict = unCredential credentials

        getProp name =
            maybe (Left $ "No " ++ ByteString.unpack name ++ " field in credential.") Right
             $ (removeQuotes . show)
            <$> lookup name dict
    in
        do
            token <- getProp "oauth_token"
            tokenSecret <- getProp "oauth_token_secret"
            uId <- getProp "user_id"
            sName <- getProp "screen_name"
            return
                UserDetails
                    { oauthToken = token
                    , oauthTokenSecret = tokenSecret
                    , userId = uId
                    , screenName = sName
                    , accessRequestToken = requestToken
                    }

removeQuotes :: [a] -> [a]
removeQuotes v =
    drop 1 $ take (length v - 1) v


saveUserDetails :: UserDetails -> Action IO Bson.Value
saveUserDetails userDetails =
    let
        collection = "credentials"
        document = toBSON userDetails
    in
        Mongo.insert collection document

redirectTo :: String -> Handler a
redirectTo url =
    let
        locationHeader = (hLocation, ByteString.pack url)
        otherHeaders = errHeaders err301
        err = err301
            { errHeaders = locationHeader:otherHeaders }
    in
        throwError err
