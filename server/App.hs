{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module App (runApp) where

import Api (Api)
import Network.Wai.Handler.Warp (run)
import Servant ((:<|>) ((:<|>)), Application, Proxy (Proxy), Raw, Server, serve)
import Servant.Utils.StaticFiles (serveDirectoryFileServer)
import Types (EnvironmentVariables (port), InfoMsg (InfoMsg))


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

-- App

app :: IO Application
app = serve withAssetsProxy <$> server

runApp :: EnvironmentVariables -> IO ()
runApp env = do
    putStrLn $ "Running server on port " ++ (show $ port env)
    v <- app
    run (port env) v
