{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Routes.UserSearch (get) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import qualified Data.ByteString.Lazy.Char8 as LB
import MongoTypes.UserDetails (UserDetails)
import Network.HTTP.Client (Manager)
import Servant (Handler, err500, errBody, throwError)
import Twitter (searchUser)
import qualified Web.Authenticate.OAuth as OAuth


get :: OAuth.OAuth -> Manager -> UserDetails -> Maybe String -> Handler Value
get oauth manager userDetails mQ =
    liftIO fetchUserSearch >>= either err return
        where
            err :: String -> Handler a
            err msg =
                throwError $ err500 { errBody = LB.pack msg }

            fetchUserSearch =
                searchUser
                    oauth
                    manager
                    userDetails
                    mQ
