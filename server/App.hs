{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module App (runApp) where

import Api (Api, apiServer)
import Authenticate (authContext)
import Data.Text (Text)
import qualified Data.Text as T
import Database.MongoDB (Pipe, access, connect, master, readHostPort)
import qualified Database.MongoDB.Query as Query
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
    (cors, corsRequestHeaders, simpleCorsResourcePolicy)
import Servant
    ( (:<|>) ((:<|>))
    , Application
    , Proxy (Proxy)
    , Raw
    , Server
    , serveWithContext
    )
import Servant.Utils.StaticFiles (serveDirectoryFileServer)
import Types (DBActionRunner, EnvironmentVariables (dbName, dbUrl, port))

-------------------------------------------------------------------------------
--                               App
-------------------------------------------------------------------------------


runApp :: EnvironmentVariables -> IO ()
runApp env = do
    putStrLn $ "Running server on port " ++ show (port env)
    pipe <- getDbPipe (dbUrl env)
    manager <- newTlsManager
    let dbRunner = generateRunner pipe (dbName env)
        serverApp = app env dbRunner manager
        portName = port env
    run portName serverApp

getDbPipe :: Text -> IO Pipe
getDbPipe databaseUrl =
    connect $readHostPort $ T.unpack databaseUrl

generateRunner :: Pipe -> Query.Database -> DBActionRunner
generateRunner pipe database =
    access pipe master database


app :: EnvironmentVariables -> DBActionRunner -> Manager -> Application
app env dbRunner manager =
    serveWithCORS $
        serveWithContext
            withAssetsProxy
            (authContext dbRunner)
            $ server env dbRunner manager

{- Allow CORS and allow certain headers in CORS requests -}
serveWithCORS :: Middleware
serveWithCORS =
    cors $ const $ Just corsPolicy
        where
            corsPolicy =
                simpleCorsResourcePolicy
                    { corsRequestHeaders = ["x-app-token"]
                    }

-------------------------------------------------------------------------------
--                               Routes
-------------------------------------------------------------------------------

type WithAssets = Api :<|> Raw

withAssetsProxy :: Proxy WithAssets
withAssetsProxy =
        Proxy

server :: EnvironmentVariables -> DBActionRunner -> Manager -> Server WithAssets
server env dbRunner manager =
    apiServer env dbRunner manager :<|> serveAssets

serveAssets :: Server Raw
serveAssets =
    serveDirectoryFileServer "./static/build"
