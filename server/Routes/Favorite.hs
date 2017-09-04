{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Routes.Favorite (post, delete) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import qualified Data.ByteString.Lazy.Char8 as LB
import MongoTypes.UserDetails (UserDetails)
import Network.HTTP.Client (Manager)
import Servant (Handler, err500, errBody, throwError)
import Twitter (postFavorite)
import qualified Web.Authenticate.OAuth as OAuth


post :: OAuth.OAuth -> Manager -> UserDetails -> Maybe String -> Handler Value
post = favorite True


delete :: OAuth.OAuth -> Manager -> UserDetails -> Maybe String -> Handler Value
delete = favorite False


favorite :: Bool -> OAuth.OAuth -> Manager -> UserDetails -> Maybe String -> Handler Value
favorite isFavortite oauth manager userDetails mTweetId =
    liftIO (postFavorite oauth manager userDetails isFavortite mTweetId)
    >>= either err return
        where
            err :: String -> Handler a
            err msg =
                throwError $ err500 { errBody = LB.pack msg }
