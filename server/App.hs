{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module App (runApp) where
import Api (Api, apiServer)
import Authenticate (authContext)
import Data.Bool (bool)
import qualified Data.Text as T
import Database.MongoDB
    (Host (Host), Pipe, PortID (PortNumber), access, auth, connect, master)
import qualified Database.MongoDB.Query as Query
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
    (cors, corsMethods, corsRequestHeaders, simpleCorsResourcePolicy)
import Servant
    ( (:<|>) ((:<|>))
    , Application
    , Proxy (Proxy)
    , Raw
    , Server
    , serveWithContext
    )
import Servant.Utils.StaticFiles (serveDirectoryFileServer)
import Types
    ( DBActionRunner
    , EnvironmentVariables (dbName, dbPassword, dbPort, dbUrl, dbUsername, port)
    )

-------------------------------------------------------------------------------
--                               App
-------------------------------------------------------------------------------


runApp :: EnvironmentVariables -> IO ()
runApp env = do
    putStrLn $ "Running server on port " ++ show (port env)

    -- DATABASE SETUP
    pipe <- connectToMongo env
    let runDbAction = createActionRunner pipe (dbName env)
    authenticated <- authenticateConnection env runDbAction
    putStrLn $ "Database authentication " ++ bool "failed" "succeeded" authenticated

    -- SERVER START
    manager <- newTlsManager
    let
        serverApp = app env runDbAction manager
        portName = port env
    run portName serverApp


connectToMongo :: EnvironmentVariables -> IO Pipe
connectToMongo env =
    connect
        $ Host (T.unpack $ dbUrl env)
        $ PortNumber (read $ show $ dbPort env)

-- After we generate a Pipe, we need to authenticate that pipe for it to work
authenticateConnection :: EnvironmentVariables -> DBActionRunner -> IO Bool
authenticateConnection env runDbAction =
    runDbAction $ auth (dbUsername env) (dbPassword env)


createActionRunner :: Pipe -> Query.Database -> DBActionRunner
createActionRunner pipe database =
    access pipe master database


app :: EnvironmentVariables -> DBActionRunner -> Manager -> Application
app env runDbAction manager =
    serveWithCORS $
        serveWithContext
            withAssetsProxy
            (authContext runDbAction)
            $ server env runDbAction manager

{- Allow CORS and allow certain headers in CORS requests -}
serveWithCORS :: Middleware
serveWithCORS =
    cors $ const $ Just corsPolicy
        where
            corsPolicy =
                simpleCorsResourcePolicy
                    { corsRequestHeaders = ["x-app-token", "content-type"]
                    , corsMethods = ["GET", "POST", "DELETE"]
                    }

-------------------------------------------------------------------------------
--                               Routes
-------------------------------------------------------------------------------

type WithAssets = Api :<|> Raw

withAssetsProxy :: Proxy WithAssets
withAssetsProxy =
        Proxy

server :: EnvironmentVariables -> DBActionRunner -> Manager -> Server WithAssets
server env runDbAction manager =
    apiServer env runDbAction manager :<|> serveAssets

serveAssets :: Server Raw
serveAssets =
    serveDirectoryFileServer "./static/build"
