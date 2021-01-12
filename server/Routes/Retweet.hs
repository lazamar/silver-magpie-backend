{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Routes.Retweet (post, delete) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import qualified Data.ByteString.Lazy.Char8 as LB
import Types.UserDetails (UserDetails)
import Network.HTTP.Client (Manager)
import Servant (Handler, err500, errBody, throwError)
import Twitter (postRetweet)
import qualified Web.Authenticate.OAuth as OAuth
import Types (HandlerM)

post :: HandlerM m => OAuth.OAuth -> Manager -> UserDetails -> Maybe String -> m Value
post = retweet True


delete :: HandlerM m => OAuth.OAuth -> Manager -> UserDetails -> Maybe String -> m Value
delete = retweet False


retweet :: forall m. HandlerM m => Bool -> OAuth.OAuth -> Manager -> UserDetails -> Maybe String -> m Value
retweet shouldRetweet oauth manager userDetails mTweetId =
    liftIO (postRetweet oauth manager userDetails shouldRetweet mTweetId)
    >>= either err return
        where
            err :: String -> m a
            err msg =
                throwError $ err500 { errBody = LB.pack msg }
