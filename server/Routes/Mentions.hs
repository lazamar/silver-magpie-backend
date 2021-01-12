{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Routes.Mentions (get) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as LB
import Types.UserDetails (UserDetails)
import Network.HTTP.Client (Manager)
import Servant (Handler, err500, errBody, throwError)
import Twitter (Timeline, WhichTimeline (MentionsTimeline), fetchTimeline)
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
    liftIO fetchMentions >>= either err return
        where
            err :: String -> m a
            err msg =
                throwError $ err500 { errBody = LB.pack msg }

            fetchMentions =
                fetchTimeline
                    MentionsTimeline
                    mMaxId
                    mSinceId
                    oauth
                    manager
                    userDetails
