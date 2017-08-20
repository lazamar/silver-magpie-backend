{-# LANGUAGE OverloadedStrings #-}

module Twitter
    ( queryApi
    , configOauth
    , configManager
    , configUserDetails
    , configUrl
    , configMethod
    , configQuery
    , RequestConfig(RequestConfig)
    ) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.List (intercalate)
import MongoTypes.UserDetails (UserDetails, oauthToken, oauthTokenSecret)
import Network.HTTP.Client
    (Manager, Response, httpLbs, method, parseRequest, queryString)
import SafeHttp (safeRequest)
import qualified Web.Authenticate.OAuth as OAuth

data RequestConfig =
    RequestConfig
        { configOauth       :: OAuth.OAuth
        , configManager     :: Manager
        , configUserDetails :: UserDetails
        , configUrl         :: String
        , configMethod      :: String
        , configQuery       :: [ (String, Maybe String) ]
        }

queryApi :: RequestConfig -> IO (Either String (Response LB.ByteString))
queryApi (RequestConfig oauth manager userDetails url cMethod query) =
    safeRequest $
        fmap Right $
        flip httpLbs manager =<<
        OAuth.signOAuth oauth credentials =<<
        withConfig <$>
        parseRequest url
    where
        queryS =
            intercalate "&" .
            filterMap id .
            fmap toQueryVariable $
            query

        withConfig request =
            request
                { method = B.pack cMethod
                , queryString = B.pack queryS
                }

        credentials =
            OAuth.insert "oauth_token_secret" (B.pack $ oauthTokenSecret userDetails)
            $ OAuth.insert "oauth_token" (B.pack $ oauthToken userDetails)
            OAuth.emptyCredential


toQueryVariable :: (String, Maybe String) -> Maybe String
toQueryVariable (_, Nothing) = Nothing
toQueryVariable (a, Just b) =
    Just $ a ++ "=" ++ b


filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f xs =
    foldr (maybeCons f) [] xs


maybeCons :: (a -> Maybe b) -> a -> [b] -> [b]
maybeCons f x ys =
    case f x of
        Nothing ->
            ys
        Just y ->
            y:ys
