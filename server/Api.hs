{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Api (Api, apiServer) where

import Control.Monad.IO.Class (liftIO)
import Data.Bson (Document)
import Database.MongoDB (Action, rest, select)
import qualified Database.MongoDB.Query as Query
import Servant ((:<|>) ((:<|>)), (:>), Get, Handler, JSON, QueryParam, Server)
import Types (DBActionRunner, EnvironmentVariables, InfoMsg (InfoMsg))
-------------------------------------------------------------------------------
--                               API
-------------------------------------------------------------------------------

type Api =  "sign-in" :> QueryParam "app_session_id" String :> Get '[JSON] InfoMsg
    :<|>    "test2" :> Get '[JSON] InfoMsg

-------------------------------------------------------------------------------
--                               Handlers
-------------------------------------------------------------------------------


getFindOne :: Action IO (Maybe Document)
getFindOne = Query.findOne $ select [] "credentials"


getCursor :: Action IO Query.Cursor
getCursor = Query.find $ select [] "credentials"


apiServer :: EnvironmentVariables -> DBActionRunner -> Server Api
apiServer env runDbAction =
    signIn env runDbAction :<|> test2 runDbAction


signIn ::  EnvironmentVariables -> DBActionRunner -> Maybe String -> Handler InfoMsg
signIn env runDbAction mAppSessionId = liftIO $ do
    theOne <- runDbAction getFindOne
    print $ show env
    print (show mAppSessionId)
    return $ InfoMsg  (show theOne)


test2 :: DBActionRunner -> Handler InfoMsg
test2 runDbAction = liftIO $ do
    cursor <- runDbAction getCursor
    vals <- runDbAction $ rest cursor
    return $ InfoMsg  (show vals)
