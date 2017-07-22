{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api (Api, server) where

import Servant ((:<|>) ((:<|>)), (:>), Get, JSON, Raw, Server)
import Servant.Utils.StaticFiles (serveDirectoryWebApp)
import Types (InfoMsg (InfoMsg))

-------------------------------------------------------------------------------
--                               API
-------------------------------------------------------------------------------

type Api = HomeStatic
    :<|> "test" :> Get '[JSON] InfoMsg

type HomeStatic = Raw


-------------------------------------------------------------------------------
--                               Handlers
-------------------------------------------------------------------------------

server :: Server Api
server = homeStatic
    :<|> getTest
    where
        homeStatic =
            serveDirectoryWebApp "./static/build"

        getTest =
            return $ InfoMsg "You got it!"
