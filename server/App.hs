{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module App (runApp) where

import Api (Api)
import Control.Monad.IO.Class (liftIO)
import Data.Bson (Document)
import Data.Text (Text)
import qualified Data.Text as T
import Database.MongoDB
    (Action, Pipe, access, connect, master, readHostPort, select)
import qualified Database.MongoDB.Query as Query
import Network.Wai.Handler.Warp (run)
import Servant
    ((:<|>) ((:<|>)), Application, Handler, Proxy (Proxy), Raw, Server, serve)
import Servant.Utils.StaticFiles (serveDirectoryFileServer)
import Types (EnvironmentVariables (dbName, dbUrl, port), InfoMsg (InfoMsg))

-------------------------------------------------------------------------------
--                               App
-------------------------------------------------------------------------------

type DBActionRunner = (forall a. Action IO a -> IO a)

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

-------------------------------------------------------------------------------
--                               Handlers
-------------------------------------------------------------------------------

serveAssets :: Server Raw
serveAssets =
    serveDirectoryFileServer "./static/build"


getFindOne :: Action IO (Maybe Document)
getFindOne = Query.findOne $ select [] "credentials"

getCursor :: Action IO (Query.Cursor)
getCursor = Query.find $ select [] "credentials"

apiServer :: DBActionRunner -> Handler InfoMsg
apiServer dbDriver = do
    theOne <- liftIO $ dbDriver getFindOne
    liftIO $ putStrLn (show theOne)
    _ <- liftIO $ dbDriver getCursor
    -- liftIO $ putStrLn (show v)
    -- liftIO $ putStrLn (show c)
    -- _ <- liftIO $ rest cursor
    return $ InfoMsg "asdf"
