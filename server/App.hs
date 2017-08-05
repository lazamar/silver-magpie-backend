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
--                               Routes
-------------------------------------------------------------------------------

type WithAssets = Api :<|> Raw

withAssetsProxy :: Proxy WithAssets
withAssetsProxy =
        Proxy

server :: IO (Server WithAssets)
server = do
    pipe <- liftIO $ connect $ readHostPort $ "127.0.0.1"
    let dbDriver = runDbAction pipe "silver-magpie"
    return $ apiServer dbDriver :<|> serveAssets

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

apiServer :: DBDriver -> Handler InfoMsg
apiServer dbDriver = do
    _ <- liftIO $ dbDriver getFindOne
    _ <- liftIO $ dbDriver getCursor
    -- liftIO $ putStrLn (show v)
    -- liftIO $ putStrLn (show c)
    -- _ <- liftIO $ rest cursor
    return $ InfoMsg (show "asdf")


-------------------------------------------------------------------------------
--                               App
-------------------------------------------------------------------------------

-- generateDBAccess :: Text -> Text -> IO (Action IO a -> IO a)
-- generateDBAccess databaseUrl databaseName = do
--     pipe <- connect $ readHostPort $ T.unpack databaseUrl
--     return $ access pipe master databaseName


type DBDriver = (forall a. Action IO a -> IO a)

runDbAction :: Pipe -> Query.Database -> DBDriver
runDbAction pipe dbName action =
    access pipe master dbName action

-- App

app :: IO Application
app = serve withAssetsProxy <$> server

runApp :: EnvironmentVariables -> IO ()
runApp env = do
    putStrLn $ "Running server on port " ++ (show $ port env)
    v <- app
    run (port env) v
