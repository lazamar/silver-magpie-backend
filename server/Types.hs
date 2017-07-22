{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson.Types (ToJSON)
import GHC.Generics (Generic)

newtype InfoMsg = InfoMsg { msg :: String }
    deriving (Generic, Show)

instance ToJSON InfoMsg
