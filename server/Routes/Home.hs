{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Routes.Home (get) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as LB
import MongoTypes.UserDetails (UserDetails)
import Network.HTTP.Client (Manager)
import Servant (Handler, err500, errBody, throwError)
import Twitter (Timeline, WhichTimeline (HomeTimeline), fetchTimeline)
import qualified Web.Authenticate.OAuth as OAuth
import Types (HandlerM)


get :: forall m. HandlerM m
    => OAuth.OAuth
    -> Manager
    -> UserDetails
    -> Maybe String
    -> Maybe String
    -> m Timeline
get oauth manager userDetails mMaxId mSinceId =
    liftIO fetchHome >>= either err return
        where
            err :: String -> m a
            err msg =
                throwError $ err500 { errBody = LB.pack msg }

            fetchHome =
                fetchTimeline
                    HomeTimeline
                    mMaxId
                    mSinceId
                    oauth
                    manager
                    userDetails
