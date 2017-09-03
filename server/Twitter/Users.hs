module Twitter.Users (searchUser) where

import Data.Aeson (Value, decode)
import MongoTypes.UserDetails (UserDetails)
import Network.HTTP.Client (Manager, responseBody)
import Twitter.Query
    ( RequestConfig (RequestConfig)
    , configManager
    , configMethod
    , configOauth
    , configQuery
    , configUrl
    , configUserDetails
    , queryApi
    )
import qualified Web.Authenticate.OAuth as OAuth

searchUser :: OAuth.OAuth -> Manager -> UserDetails -> Maybe String -> IO (Either String Value)
searchUser oauth manager userDetails mQuery =
    do
        eitherRequest <- queryApi requestConfig
        return $ eitherRequest >>= parseValue . responseBody
    where
        requestConfig =
            RequestConfig
                { configOauth = oauth
                , configManager = manager
                , configUserDetails = userDetails
                , configUrl = "https://api.twitter.com/1.1/users/search.json"
                , configMethod = "GET"
                , configQuery =
                    [ ("q", mQuery)
                    , ("include_entities", Just "false")
                    , ("count", Just "5")
                    ]
                }

        parseValue json =
            maybe
                (Left "Failed to decode twitter response")
                Right
                (decode json :: Maybe Value)
