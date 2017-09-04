{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Routes.StatusUpdate (post, StatusBody(StatusBody)) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import Data.Aeson.Types (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy.Char8 as LB
import GHC.Generics (Generic)
import MongoTypes.UserDetails (UserDetails)
import Network.HTTP.Client (Manager)
import Servant (Handler, err500, errBody, throwError)
import Twitter (postStatusUpdate)
import qualified Web.Authenticate.OAuth as OAuth

data StatusBody =
    StatusBody
        { status                :: String
        , in_reply_to_status_id :: Maybe String
        }
            deriving (Show, Generic)

instance FromJSON StatusBody
instance ToJSON StatusBody


post :: OAuth.OAuth -> Manager -> UserDetails -> StatusBody -> Handler Value
post oauth manager userDetails statusBody =
    liftIO postUpdate >>= either err return
        where
            err :: String -> Handler a
            err msg =
                throwError $ err500 { errBody = LB.pack msg }

            postUpdate =
                postStatusUpdate
                    oauth
                    manager
                    userDetails
                    (Just $ status statusBody)
                    (in_reply_to_status_id statusBody)
