{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module App (runApp) where

import Api (Api)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.Text as T
import Database.MongoDB (Action, access, connect, master, readHostPort)
import Network.Wai.Handler.Warp (run)
import Servant ((:<|>) ((:<|>)), Application, Proxy (Proxy), Raw, Server, serve)
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
server =
    return $ apiServer :<|> serveAssets

-------------------------------------------------------------------------------
--                               Handlers
-------------------------------------------------------------------------------

serveAssets :: Server Raw
serveAssets =
    serveDirectoryFileServer "./static/build"

apiServer :: Server Api
apiServer =
    return $ InfoMsg "You got it!"


-------------------------------------------------------------------------------
--                               App
-------------------------------------------------------------------------------

generateDBAccess :: (MonadIO IO) => Text -> Text -> IO (Action IO a -> IO a)
generateDBAccess databaseUrl databaseName= do
    pipe <- connect $ readHostPort $ T.unpack databaseUrl
    return $ access pipe master databaseName

-- App

app :: IO Application
app = serve withAssetsProxy <$> server

runApp :: EnvironmentVariables -> IO ()
runApp env = do
    putStrLn $ "Running server on port " ++ (show $ port env)
    _ <- generateDBAccess (dbUrl env) (dbName env)
    v <- app
    run (port env) v
