{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Routes.Home (get, ReturnType, tweets) where

import Data.Aeson.Types (ToJSON)
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager)
import Servant (Handler, err401, throwError)
import Types (DBActionRunner)
import Web.Authenticate.OAuth as OAuth


newtype ReturnType =
    ReturnType
        { tweets :: String
        }
        deriving (Show, Generic)

instance ToJSON ReturnType


get :: OAuth.OAuth -> Manager -> DBActionRunner -> Maybe String -> Handler ReturnType
get _ _ _ Nothing = throwError err401
get _ _ _ _ =
    return $ ReturnType ""
