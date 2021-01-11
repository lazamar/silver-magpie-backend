{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Authenticate (authenticate, authContext, Authenticate, AuthContext) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bson (Document, (=:))
import Data.Either.Combinators (rightToMaybe)
import MongoTypes.AppAuth (AppAuth)
import MongoTypes.UserDetails (UserDetails)
import Network.Wai (Request, requestHeaders)
import Servant
    ( Context (EmptyContext , (:.)),
      Handler,
      err401,
      errBody,
      throwError,
    )
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.Server.Experimental.Auth
    ( AuthHandler,
      AuthServerData,
      mkAuthHandler,
    )
import Types (HandlerM, targetCollection)
import qualified Control.Monad.Database as DB
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified MongoTypes.AppAuth as AppAuth
import qualified MongoTypes.UserDetails as UserDetails

-- Name of my authentication scheme. I could have multiple different ones
type Authenticate = AuthProtect "x-auth"

-- Specify what my authentication scheme will return
type instance AuthServerData (Authenticate) = UserDetails

type AuthContext = '[AuthHandler Request UserDetails]

type M m =
    ( HandlerM m
    , DB.FromRecord m AppAuth
    , DB.ToValue m String
    , DB.FromRecord m UserDetails
    )

authContext :: M m => (forall a. m a -> Handler a) -> Context AuthContext
authContext runWrapper =
    mkAuthHandler (runWrapper . handleReq) :. EmptyContext

-- Logic to authenticate a request
handleReq :: forall m. M m => Request -> m UserDetails
handleReq req =
    case lookup "x-app-token" (requestHeaders req) of
        Nothing ->
            unauthorised "Missing x-app-token auth header"
        Just authKey -> do
            mUser <- authenticate $ B.unpack authKey
            case mUser of
                Nothing -> unauthorised "Invalid authorisation token"
                Just user -> return user
    where
        unauthorised :: LB.ByteString -> m a
        unauthorised msg =
            throwError (err401 {errBody = msg})

-- Actually fetch stuff from database
authenticate :: M m => String -> m (Maybe UserDetails)
authenticate sessionId = do
    mAppAuth <- getAppAuthBySessionId sessionId
    case mAppAuth of
        Nothing ->
            return Nothing
        Just appAuth ->
            getUserDetailsByRequestToken $ AppAuth.accessRequestToken appAuth

getAppAuthBySessionId :: M m => String -> m (Maybe AppAuth)
getAppAuthBySessionId sessionId =
    DB.retrieveOne
        (targetCollection AppAuth.collectionName)
        [AppAuth.keyAppSessionId DB.=: sessionId]

getUserDetailsByRequestToken :: M m => String -> m (Maybe UserDetails)
getUserDetailsByRequestToken token =
    DB.retrieveOne
        (targetCollection UserDetails.collectionName)
        [UserDetails.keyAccessRequestToken DB.=: token]
