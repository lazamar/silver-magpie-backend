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
import Servant ((:<|>) ((:<|>)), Application, Proxy (Proxy), Raw, Server, serve)
import Servant.Utils.StaticFiles (serveDirectoryFileServer)
import Types (DBActionRunner, EnvironmentVariables (dbName, dbUrl, port))

-------------------------------------------------------------------------------
--                               App
-------------------------------------------------------------------------------


runApp :: EnvironmentVariables -> IO ()
runApp env = do
    putStrLn $ "Running server on port " ++ (show $ port env)
    pipe <- getDbPipe (dbUrl env)
    let dbRunner = generateRunner pipe (dbName env)
        serverApp = app dbRunner
        portName = port env
    run portName serverApp


getDbPipe :: Text -> IO Pipe
getDbPipe databaseUrl =
    connect $readHostPort $ T.unpack databaseUrl

generateRunner :: Pipe -> Query.Database -> DBActionRunner
generateRunner pipe database =
    \action -> access pipe master database action

app :: DBActionRunner -> Application
app dbRunner = serve withAssetsProxy $ server dbRunner


-------------------------------------------------------------------------------
--                               Routes
-------------------------------------------------------------------------------

type WithAssets = Api :<|> Raw

withAssetsProxy :: Proxy WithAssets
withAssetsProxy =
        Proxy

server :: DBActionRunner -> Server WithAssets
server dbRunner =
    apiServer dbRunner :<|> serveAssets

serveAssets :: Server Raw
serveAssets =
    serveDirectoryFileServer "./static/build"
