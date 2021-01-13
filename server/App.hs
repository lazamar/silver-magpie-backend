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
import Data.String (fromString)
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
import qualified Control.Monad.Database.SQLite as SQL
import qualified Data.AppAuth as AppAuth
import qualified Data.Text as T
import qualified Data.UserDetails as UserDetails

-------------------------------------------------------------------------------
--                               App
-------------------------------------------------------------------------------

runApp :: EnvironmentVariables -> IO ()
runApp env = do
    putStrLn $ "Running server on port " ++ show (port env)

    let S runMonad = databaseRunner env

    -- SERVER START
    manager <- newTlsManager
    let serverApp = app runMonad env manager
        portName = port env
    run portName serverApp

type Stack = MonadSQLT (ExceptT ServerError IO)

newtype DBRunner m n = S (forall a. m a -> n a)

-- TODO database name must be set by the environment variables
databaseRunner :: EnvironmentVariables -> DBRunner Stack Handler
databaseRunner _ = S $ \runServer -> Handler $
    runMonadSQL "./database/DB.sql" $ do
        prepareDatabase
        runServer
    where
        collections =
            [ UserDetails.collectionName
            , AppAuth.collectionName
            ]

        prepareDatabase = do
            -- Prepare database for usage
            -- Enable foreign key constraints. It's really weird that they would otherwise just not work.
            SQL.execute_ "PRAGMA foreign_keys = ON"
            -- Make sure the correct transaction tracking option is in place
            SQL.execute_ "PRAGMA journal_mode = WAL"
            SQL.execute_ "PRAGMA synchronous = NORMAL"
            createTables

        createTables = do
            SQL.execute_ $
                asQuery
                    [ "CREATE TABLE IF NOT EXISTS " <> AppAuth.collectionName <> " "
                    , "( " <> AppAuth.keyAppSessionId <> "         TEXT NOT NULL"
                    , ", " <> AppAuth.keyAccessRequestToken <> "   TEXT NOT NULL"
                    , ")"
                    ]

            SQL.execute_ $
                asQuery
                    [ "CREATE TABLE IF NOT EXISTS " <> UserDetails.collectionName <> " "
                    , "( " <> UserDetails.keyOauthToken <> "         TEXT NOT NULL"
                    , ", " <> UserDetails.keyOauthTokenSecret <> "   TEXT NOT NULL"
                    , ", " <> UserDetails.keyUserId <> "             TEXT NOT NULL"
                    , ", " <> UserDetails.keyScreenName <> "         TEXT NOT NULL"
                    , ", " <> UserDetails.keyAccessRequestToken <> " TEXT NOT NULL"
                    , ")"
                    ]
        asQuery = fromString . T.unpack . T.unwords

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
