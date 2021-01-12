{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Routes.Favorite (post, delete) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.UserDetails (UserDetails)
import Network.HTTP.Client (Manager)
import Servant (err500, errBody, throwError)
import Twitter (postFavorite)
import qualified Web.Authenticate.OAuth as OAuth
import Types (HandlerM)

post :: HandlerM m => OAuth.OAuth -> Manager -> UserDetails -> Maybe String -> m Value
post = favorite True


delete :: HandlerM m => OAuth.OAuth -> Manager -> UserDetails -> Maybe String -> m Value
delete = favorite False


favorite :: forall m. HandlerM m => Bool -> OAuth.OAuth -> Manager -> UserDetails -> Maybe String -> m Value
favorite isFavortite oauth manager userDetails mTweetId =
    liftIO (postFavorite oauth manager userDetails isFavortite mTweetId)
    >>= either err return
        where
            err :: String -> m a
            err msg =
                throwError $ err500 { errBody = LB.pack msg }
