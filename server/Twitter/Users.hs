module Twitter.Users (searchUser) where

import Data.Aeson (Value)
import MongoTypes.UserDetails (UserDetails)
import Network.HTTP.Client (Manager)
import qualified Twitter.Query
import qualified Web.Authenticate.OAuth as OAuth

searchUser :: OAuth.OAuth -> Manager -> UserDetails -> Maybe String -> IO (Either String Value)
searchUser oauth manager userDetails mQuery =
    Twitter.Query.get
        "https://api.twitter.com/1.1/users/search.json"
        [ ("q", mQuery)
        , ("include_entities", Just "false")
        , ("count", Just "5")
        ]
        oauth
        manager
        userDetails
