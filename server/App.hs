{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module App (runApp) where

import Api (Api, apiServer)
import Data.Text (Text)
import qualified Data.Text as T
import Database.MongoDB (Pipe, access, connect, master, readHostPort)
import qualified Database.MongoDB.Query as Query
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant ((:<|>) ((:<|>)), Application, Proxy (Proxy), Raw, Server, serve)
import Servant.Utils.StaticFiles (serveDirectoryFileServer)
import Types (DBActionRunner, EnvironmentVariables (dbName, dbUrl, port))
-------------------------------------------------------------------------------
--                               App
-------------------------------------------------------------------------------


runApp :: EnvironmentVariables -> IO ()
runApp env = do
    putStrLn $ "Running server on port " ++ show (port env)
    pipe <- getDbPipe (dbUrl env)
    let dbRunner = generateRunner pipe (dbName env)
        serverApp = app env dbRunner
        portName = port env
    run portName serverApp


getDbPipe :: Text -> IO Pipe
getDbPipe databaseUrl =
    connect $readHostPort $ T.unpack databaseUrl

generateRunner :: Pipe -> Query.Database -> DBActionRunner
generateRunner pipe database =
    access pipe master database

app :: EnvironmentVariables -> DBActionRunner -> Application
app env dbRunner = simpleCors $ serve withAssetsProxy $ server env dbRunner


-------------------------------------------------------------------------------
--                               Routes
-------------------------------------------------------------------------------

type WithAssets = Api :<|> Raw

withAssetsProxy :: Proxy WithAssets
withAssetsProxy =
        Proxy

server :: EnvironmentVariables -> DBActionRunner -> Server WithAssets
server env dbRunner =
    apiServer env dbRunner :<|> serveAssets

serveAssets :: Server Raw
serveAssets =
    serveDirectoryFileServer "./static/build"
