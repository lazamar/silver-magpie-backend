{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App (runApp) where

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Conc.Class (MonadConc)
import Api (Api, apiServer)
import Authenticate (authContext, AuthContext)
import Control.Monad.Database.SQLite
import Data.Bool (bool)
import Database.MongoDB
    ( Host (Host),
      Pipe,
      PortID (PortNumber),
      access,
      auth,
      connect,
      master,
    )
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
    ( Application
    , Proxy (Proxy)
    , Raw
    , Server
    , Handler(..)
    , ServerT(..)
    , ServerError(..)
    , serveWithContext
    , hoistServerWithContext
    , hoistServer
    , (:<|>) ((:<|>))
    )
import Servant.Server.StaticFiles (serveDirectoryFileServer)
import Types
    ( HandlerM
    , DBActionRunner
    , EnvironmentVariables (dbName , dbPassword , dbPort , dbUrl , dbUsername , port),
    )
import qualified Data.Text as T
import qualified Database.MongoDB.Query as Query

-------------------------------------------------------------------------------
--                               App
-------------------------------------------------------------------------------


runApp :: EnvironmentVariables -> IO ()
runApp env = do
    putStrLn $ "Running server on port " ++ show (port env)

    let S runMonad = stackRunner env
    let runDbAction = undefined

    -- SERVER START
    manager <- newTlsManager
    let serverApp = app runMonad env runDbAction manager
        portName = port env
    run portName serverApp

type Stack = MonadSQLT (ExceptT ServerError IO)

newtype StackRunner m n = S (forall a. m a -> n a)

-- TODO database name must be set by the environment variables
stackRunner :: EnvironmentVariables -> StackRunner Stack Handler
stackRunner _ = S $ Handler . runMonadSQL "./database/DB.sql"

connectToMongo :: EnvironmentVariables -> IO Pipe
connectToMongo env =
    connect $
        Host (T.unpack $ dbUrl env) $
            PortNumber (read $ show $ dbPort env)

-- After we generate a Pipe, we need to authenticate that pipe for it to work
-- authenticateConnection :: HandlerM m => EnvironmentVariables -> DBActionRunner m -> m Bool
-- authenticateConnection env runDbAction =
--     runDbAction $ auth (dbUsername env) (dbPassword env)

-- createActionRunner :: Pipe -> Query.Database -> DBActionRunner m
-- createActionRunner pipe database =
--     access pipe master database

app :: ()
    => (forall a. Stack a -> Handler a)
    -> EnvironmentVariables
    -> DBActionRunner Stack
    -> Manager
    -> Application
app runMonad env runDbAction manager =
    serveWithCORS $
        serveWithContext
            withAssetsProxy
            (authContext runMonad)
            $ server runMonad env runDbAction manager


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

server :: ()
    => (forall a. Stack a -> Handler a)
    -> EnvironmentVariables
    -> DBActionRunner Stack
    -> Manager
    -> Server WithAssets
server runMonad env runDbAction manager =
    hoistServerWithContext
        (Proxy :: Proxy WithAssets)
        (Proxy :: Proxy AuthContext)
        runMonad
        theServer
    where
        theServer :: ServerT WithAssets Stack
        theServer = apiServer env runDbAction manager :<|> serveAssets

serveAssets :: ServerT Raw m
serveAssets =
    serveDirectoryFileServer "./static/build"
