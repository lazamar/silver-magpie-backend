module Main where

import Api (Api, server)
import Network.Wai.Handler.Warp (run)
import Servant (Application, Proxy (Proxy), serve)
import Types (InfoMsg (InfoMsg))

-------------------------------------------------------------------------------
--                               App
-------------------------------------------------------------------------------

proxyApi :: Proxy Api
proxyApi = Proxy

-- App

app :: Application
app = serve proxyApi server

-------------------------------------------------------------------------------
--                               Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    let port = 8081
    putStrLn $ "Running server on port " ++ (show port)
    run port app
