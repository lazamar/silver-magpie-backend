{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Routes.UserSearch (get) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import qualified Data.ByteString.Lazy.Char8 as LB
import Types.UserDetails (UserDetails)
import Network.HTTP.Client (Manager)
import Servant (err500, errBody, throwError)
import Twitter (searchUser)
import qualified Web.Authenticate.OAuth as OAuth
import Types (HandlerM)



get :: forall m. HandlerM m => OAuth.OAuth -> Manager -> UserDetails -> Maybe String -> m Value
get oauth manager userDetails mQ =
    liftIO fetchUserSearch >>= either err return
        where
            err :: String -> m a
            err msg =
                throwError $ err500 { errBody = LB.pack msg }

            fetchUserSearch =
                searchUser
                    oauth
                    manager
                    userDetails
                    mQ
