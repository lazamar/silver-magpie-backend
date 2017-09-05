{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Routes.Retweet (post, delete) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import qualified Data.ByteString.Lazy.Char8 as LB
import MongoTypes.UserDetails (UserDetails)
import Network.HTTP.Client (Manager)
import Servant (Handler, err500, errBody, throwError)
import Twitter (postRetweet)
import qualified Web.Authenticate.OAuth as OAuth


post :: OAuth.OAuth -> Manager -> UserDetails -> Maybe String -> Handler Value
post = retweet True


delete :: OAuth.OAuth -> Manager -> UserDetails -> Maybe String -> Handler Value
delete = retweet False


retweet :: Bool -> OAuth.OAuth -> Manager -> UserDetails -> Maybe String -> Handler Value
retweet shouldRetweet oauth manager userDetails mTweetId =
    liftIO (postRetweet oauth manager userDetails shouldRetweet mTweetId)
    >>= either err return
        where
            err :: String -> Handler a
            err msg =
                throwError $ err500 { errBody = LB.pack msg }
