{-# LANGUAGE OverloadedStrings #-}

module Twitter.Query
    ( queryApi
    , query
    , get
    ) where

import Data.Aeson (Value, decode)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.List (intercalate)
import MongoTypes.UserDetails (UserDetails, oauthToken, oauthTokenSecret)
import Network.HTTP.Client
    ( Manager
    , Response
    , httpLbs
    , method
    , parseRequest
    , queryString
    , responseBody
    )
import SafeHttp (safeRequest)
import qualified Web.Authenticate.OAuth as OAuth


get :: String -> [(String, Maybe String)] -> OAuth.OAuth -> Manager -> UserDetails -> IO (Either String Value)
get = query "GET"


query ::  String -> String -> [(String, Maybe String)] -> OAuth.OAuth -> Manager -> UserDetails -> IO (Either String Value)
query meth url queryList oauth manager userDetails =
    do
        eitherRequest <- queryApi meth url queryList oauth manager userDetails
        return $ eitherRequest >>= parseValue . responseBody
    where
        parseValue json =
            maybe
                (Left "Failed to decode twitter response")
                Right
                (decode json :: Maybe Value)


queryApi ::  String -> String -> [(String, Maybe String)] -> OAuth.OAuth -> Manager -> UserDetails  -> IO (Either String (Response LB.ByteString))
queryApi  cMethod url queryList oauth manager userDetails =
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
            queryList

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
