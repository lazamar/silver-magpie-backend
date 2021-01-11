{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module App (runApp) where

import Api (Api, apiServer)
import Authenticate (AuthContext, authContext)
import Control.Monad.Conc.Class (MonadConc)
import Control.Monad.Database.SQLite
import Control.Monad.Except (ExceptT (..))
import Data.Bool (bool)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
    ( cors,
      corsMethods,
      corsRequestHeaders,
      simpleCorsResourcePolicy,
    )
import Servant
    ( Application,
      Handler (..),
      Proxy (Proxy),
      Raw,
      Server,
      ServerError (..),
      ServerT (..),
      hoistServer,
      hoistServerWithContext,
      serveWithContext,
      (:<|>) ((:<|>)),
    )
import Servant.Server.StaticFiles (serveDirectoryFileServer)
import Types
    ( EnvironmentVariables (dbName , dbPassword , dbPort , dbUrl , dbUsername , port),
      HandlerM,
    )
import qualified Data.Text as T
import qualified MongoTypes.AppAuth as AppAuth
import qualified MongoTypes.UserDetails as UserDetails

-------------------------------------------------------------------------------
--                               App
-------------------------------------------------------------------------------

runApp :: EnvironmentVariables -> IO ()
runApp env = do
    putStrLn $ "Running server on port " ++ show (port env)

    let S runMonad = stackRunner env

    -- SERVER START
    manager <- newTlsManager
    let serverApp = app runMonad env manager
        portName = port env
    run portName serverApp

type Stack = MonadSQLT (ExceptT ServerError IO)

newtype StackRunner m n = S (forall a. m a -> n a)

-- TODO database name must be set by the environment variables
stackRunner :: EnvironmentVariables -> StackRunner Stack Handler
stackRunner _ = S $ Handler . runMonadSQL "./database/DB.sql"
    where
        collections =
            [ UserDetails.collectionName
            , AppAuth.collectionName
            ]

--connectToMongo :: EnvironmentVariables -> IO Pipe
--connectToMongo env =
--connect $
--Host (T.unpack $ dbUrl env) $
--PortNumber (read $ show $ dbPort env)

-- After we generate a Pipe, we need to authenticate that pipe for it to work
-- authenticateConnection :: HandlerM m => EnvironmentVariables -> DBActionRunner m -> m Bool
-- authenticateConnection env runDbAction =
--     runDbAction $ auth (dbUsername env) (dbPassword env)

-- createActionRunner :: Pipe -> Query.Database -> DBActionRunner m
-- createActionRunner pipe database =
--     access pipe master database

app
    :: ()
    => (forall a. Stack a -> Handler a)
    -> EnvironmentVariables
    -> Manager
    -> Application
app runMonad env manager =
    serveWithCORS $
        serveWithContext
            withAssetsProxy
            (authContext runMonad)
            $ server runMonad env manager

{- Allow CORS and allow certain headers in CORS requests -}
serveWithCORS :: Middleware
serveWithCORS =
    cors $ const $ Just corsPolicy
    where
        corsPolicy =
            simpleCorsResourcePolicy
                { corsRequestHeaders = ["x-app-token" , "content-type"]
                , corsMethods = ["GET" , "POST" , "DELETE"]
                }

-------------------------------------------------------------------------------
--                               Routes
-------------------------------------------------------------------------------

type WithAssets = Api :<|> Raw

withAssetsProxy :: Proxy WithAssets
withAssetsProxy =
    Proxy

server
    :: ()
    => (forall a. Stack a -> Handler a)
    -> EnvironmentVariables
    -> Manager
    -> Server WithAssets
server runMonad env manager =
    hoistServerWithContext
        (Proxy :: Proxy WithAssets)
        (Proxy :: Proxy AuthContext)
        runMonad
        theServer
    where
        theServer :: ServerT WithAssets Stack
        theServer = apiServer env manager :<|> serveAssets

serveAssets :: ServerT Raw m
serveAssets =
    serveDirectoryFileServer "./static/build"
