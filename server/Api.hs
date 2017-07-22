{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api (Api) where

import Servant ((:>), Get, JSON)
import Types (InfoMsg)

-------------------------------------------------------------------------------
--                               API
-------------------------------------------------------------------------------

type Api =  "test" :> Get '[JSON] InfoMsg
