module Server (runServer) where

import Api (Api, server)
import Network.Wai.Handler.Warp (run)
import Servant (Application, Proxy (Proxy), serve)

import Types (EnvironmentVariables (port))

-------------------------------------------------------------------------------
--                               App
-------------------------------------------------------------------------------

proxyApi :: Proxy Api
proxyApi = Proxy

-- App

app :: Application
app = serve proxyApi server

runServer :: EnvironmentVariables -> IO ()
runServer env = do
    putStrLn $ "Running server on port " ++ (show $ port env)
    run (port env) app
