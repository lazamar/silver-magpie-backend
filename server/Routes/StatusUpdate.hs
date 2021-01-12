{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Routes.StatusUpdate (post, StatusBody(StatusBody)) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import Data.Aeson.Types (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy.Char8 as LB
import GHC.Generics (Generic)
import Data.UserDetails (UserDetails)
import Network.HTTP.Client (Manager)
import Servant (Handler, err500, errBody, throwError)
import Twitter (postStatusUpdate)
import qualified Web.Authenticate.OAuth as OAuth
import Types (HandlerM)

data StatusBody =
    StatusBody
        { status                :: String
        , in_reply_to_status_id :: Maybe String
        }
            deriving (Show, Generic)

instance FromJSON StatusBody
instance ToJSON StatusBody


post :: forall m. HandlerM m => OAuth.OAuth -> Manager -> UserDetails -> StatusBody -> m Value
post oauth manager userDetails statusBody =
    liftIO postUpdate >>= either err return
        where
            err :: String -> m a
            err msg =
                throwError $ err500 { errBody = LB.pack msg }

            postUpdate =
                postStatusUpdate
                    oauth
                    manager
                    userDetails
                    (Just $ status statusBody)
                    (in_reply_to_status_id statusBody)
