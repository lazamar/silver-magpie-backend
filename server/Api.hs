{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Api (Api, apiServer) where

import Control.Monad.IO.Class (liftIO)
import Data.Bson (Document)
import Database.MongoDB (Action, rest, select)
import qualified Database.MongoDB.Query as Query
import Servant ((:<|>) ((:<|>)), (:>), Get, Handler, JSON, Server)
import Types (DBActionRunner, InfoMsg (InfoMsg))

-------------------------------------------------------------------------------
--                               API
-------------------------------------------------------------------------------

type Api =  "test1" :> Get '[JSON] InfoMsg
    :<|>    "test2" :> Get '[JSON] InfoMsg

-------------------------------------------------------------------------------
--                               Handlers
-------------------------------------------------------------------------------


getFindOne :: Action IO (Maybe Document)
getFindOne = Query.findOne $ select [] "credentials"


getCursor :: Action IO (Query.Cursor)
getCursor = Query.find $ select [] "credentials"


apiServer :: DBActionRunner -> Server Api
apiServer runDbAction =
    test1 runDbAction :<|> test2 runDbAction


test1 :: DBActionRunner -> Handler InfoMsg
test1 runDbAction = liftIO $ do
    theOne <- runDbAction getFindOne
    return $ InfoMsg  (show theOne)


test2 :: DBActionRunner -> Handler InfoMsg
test2 runDbAction = liftIO $ do
    cursor <- runDbAction getCursor
    vals <- runDbAction $ rest cursor
    return $ InfoMsg  (show vals)
