{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Routes.SaveCredentials (get) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either.Combinators (mapLeft)
import MongoTypes.UserDetails
    ( UserDetails (UserDetails),
      accessRequestToken,
      oauthToken,
      oauthTokenSecret,
      screenName,
      toBSON,
      userId,
    )
import Network.HTTP.Client (Manager, Response, responseBody)
import Network.HTTP.Types.Header (hLocation)
import SafeHttp (safeRequest)
import Servant
    ( Handler,
      ServerError,
      err301,
      err400,
      err500,
      errBody,
      errHeaders,
      throwError,
    )
import Types (HandlerM, InfoMsg, targetCollection)
import Web.Authenticate.OAuth (unCredential)
import qualified Control.Monad.Database as DB
import qualified Data.Bson as Bson
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy.Char8 as LByteString
import qualified MongoTypes.UserDetails as UserDetails
import qualified Web.Authenticate.OAuth as OAuth

{-
    The user is taken here after he authorises the application with twitter
    Here we save his authorisation data.
-}

get :: (HandlerM m , DB.ToRecord m UserDetails) => OAuth.OAuth -> Manager -> Maybe String -> Maybe String -> m InfoMsg
get _ _ Nothing _ = throwError err400
get _ _ _ Nothing = throwError err400
get oauth manager (Just requestToken) (Just requestVerifier) =
    do
        eitherCredentials <- liftIO $ getAccessToken manager oauth requestToken requestVerifier
        let eitherDetails = eitherCredentials >>= toUserDetails requestToken
        handleDetails eitherDetails
    where
        handleDetails (Left err) =
            throwError $ err500 {errBody = LByteString.pack err}
        handleDetails (Right userDetails) =
            saveUserDetails userDetails >> redirectTo "./thank-you.html"

getAccessToken :: Manager -> OAuth.OAuth -> String -> String -> IO (Either String OAuth.Credential)
getAccessToken manager oauth requestToken requestVerifier =
    safeRequest $
        mapLeft getStringBody
            <$> OAuth.getAccessTokenWith
                ( OAuth.defaultAccessTokenRequest
                      oauth
                      reqCredentials
                      manager
                )
    where
        reqCredentials =
            OAuth.injectVerifier (ByteString.pack requestVerifier) $
                OAuth.newCredential (ByteString.pack requestToken) ""

toUserDetails :: String -> OAuth.Credential -> Either String UserDetails
toUserDetails requestToken credentials =
    let dict = unCredential credentials

        getProp name =
            maybe (Left $ "No " ++ ByteString.unpack name ++ " field in credential.") Right $
                (removeQuotes . show)
                    <$> lookup name dict
     in do
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

saveUserDetails :: (HandlerM m , DB.ToRecord m UserDetails) => UserDetails -> m ()
saveUserDetails userDetails =
    DB.store
        (targetCollection UserDetails.collectionName)
        (DB.Insert userDetails)

redirectTo :: MonadError ServerError m => String -> m a
redirectTo url =
    let locationHeader = (hLocation , ByteString.pack url)
        otherHeaders = errHeaders err301
        err =
            err301
                { errHeaders = locationHeader : otherHeaders
                }
     in throwError err

getStringBody :: Response LByteString.ByteString -> String
getStringBody r =
    show $ responseBody r
