{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}

module Authenticate (authenticate, authContext, Authenticate, AuthContext) where


import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bson (Document, (=:))
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Either.Combinators (rightToMaybe)
import Database.MongoDB.Query (Action, Collection, Selector, findOne, select)
import MongoTypes.AppAuth (AppAuth)
import qualified MongoTypes.AppAuth as AppAuth
import MongoTypes.UserDetails (UserDetails)
import qualified MongoTypes.UserDetails as UserDetails
import Network.Wai (Request, requestHeaders)
import Servant
    (Context ((:.), EmptyContext), Handler, err401, errBody, throwError)
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.Server.Experimental.Auth
    (AuthHandler, AuthServerData, mkAuthHandler)
import Types (HandlerM, DBActionRunner)


-- Name of my authentication scheme. I could have multiple different ones
type Authenticate = AuthProtect "x-auth"

-- Specify what my authentication scheme will return
type instance AuthServerData (Authenticate) = UserDetails

type AuthContext = '[AuthHandler Request UserDetails]

authContext ::  HandlerM m => (forall a. m a -> Handler a) -> Context AuthContext
authContext runWrapper =
    mkAuthHandler (runWrapper . handleReq) :. EmptyContext


-- Logic to authenticate a request
handleReq :: forall m. HandlerM m => Request -> m UserDetails
handleReq req =
    case lookup "x-app-token" (requestHeaders req) of
        Nothing ->
          unauthorised "Missing x-app-token auth header"

        Just authKey -> undefined -- TODO
            --do
                --mUser <- runDb $ authenticate $ B.unpack authKey
                --case mUser of
                    --Nothing ->
                        --unauthorised "Invalid authorisation token"

                    --Just user ->
                        --return user
    where
        unauthorised :: LB.ByteString -> m a
        unauthorised msg =
            throwError (err401 { errBody = msg })


-- Actually fetch stuff from database
authenticate :: MonadIO m => String -> Action m (Maybe UserDetails)
authenticate sessionId =
    do
        mAppAuth <- getAppAuthBySessionId sessionId
        case mAppAuth of
            Nothing ->
                return Nothing
            Just appAuth ->
                getUserDetailsByRequestToken $ AppAuth.accessRequestToken appAuth


getAppAuthBySessionId :: MonadIO m => String -> Action m (Maybe AppAuth)
getAppAuthBySessionId sessionId =
    fetch
        [ AppAuth.keyAppSessionId =: sessionId ]
        AppAuth.collectionName
        AppAuth.fromBSON


getUserDetailsByRequestToken :: MonadIO m => String -> Action m (Maybe UserDetails)
getUserDetailsByRequestToken token =
    fetch
        [ UserDetails.keyAccessRequestToken =: token ]
        UserDetails.collectionName
        UserDetails.fromBSON


fetch :: MonadIO m => Selector -> Collection -> (Document -> Either e a) -> Action m (Maybe a)
fetch selector collection decoder =
    (=<<) (rightToMaybe . decoder) <$> findOne selection
    where
        selection =
            select selector collection
