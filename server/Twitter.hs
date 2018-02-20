{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Twitter
    ( queryApi
    , asValue
    , postFavorite
    , postStatusUpdate
    , postRetweet
    , fetchTimeline
    , tweets -- just for the compiler to be happy
    , searchUser
    , WhichTimeline (..)
    , Timeline
    ) where

import           Data.Aeson                 (Value, decode)
import           Data.Aeson.Types           (ToJSON)
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Function              ((&))
import           Data.List                  (intercalate)
import           Data.Maybe                 (fromMaybe)
import           Debug.Trace                (traceShowId)
import           GHC.Generics               (Generic)
import           MongoTypes.UserDetails     (UserDetails, oauthToken,
                                             oauthTokenSecret)
import           Network.HTTP.Client        (Manager, Response, httpLbs, method,
                                             parseRequest, queryString,
                                             responseBody)
import qualified Network.URI.Encode         as URI
import           SafeHttp                   (safeRequest)
import           Text.Read                  (readMaybe)
import           Web.Authenticate.OAuth     (OAuth)
import qualified Web.Authenticate.OAuth     as OAuth
{-
    Query the Twitter API and return the raw response as a ByteString
    Other methods will interact with the API through this one.
-}

queryApi :: String
    -> String
    -> [(String, Maybe String)]
    -> OAuth
    -> Manager
    -> UserDetails
    -> IO (Either String (Response LB.ByteString))
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
            OAuth.insert
                "oauth_token_secret"
                (B.pack $ oauthTokenSecret userDetails)
            $ OAuth.insert
                "oauth_token"
                (B.pack $ oauthToken userDetails)
                OAuth.emptyCredential


toQueryVariable :: (String, Maybe String) -> Maybe String
toQueryVariable (_, Nothing) = Nothing
toQueryVariable (a, Just b) =
    Just $ URI.encode a ++ "=" ++ URI.encode b


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

{-
    Functions to help dealing with the response from Twitter
-}

asValue ::  Either String (Response LB.ByteString) -> Either String Value
asValue eitherResponse =
    eitherResponse >>= (parseValue . responseBody)
    where
        parseValue json =
            maybe
                (Left "Failed to decode twitter response")
                Right
                (decode json :: Maybe Value)


-------------------------------------------------------------------------------
------------------------------ TIMELINE ---------------------------------------
-------------------------------------------------------------------------------

newtype Timeline =
    Timeline
        { tweets :: Value
        }
        deriving (Show, Generic)

instance ToJSON Timeline


data WhichTimeline
    = HomeTimeline
    | MentionsTimeline


instance Show WhichTimeline where
    show HomeTimeline     = "home_timeline"
    show MentionsTimeline = "mentions_timeline"


fetchTimeline :: WhichTimeline
    -> Maybe String
    -> Maybe String
    -> OAuth
    -> Manager
    -> UserDetails
    -> IO (Either String Timeline)
fetchTimeline timeline mSinceId mMaxId oauth manager userDetails  =
    fmap Timeline <$>
    asValue <$>
        queryApi
            "GET"
            url
            queryList
            oauth
            manager
            userDetails

    where
        queryList =
            [ ("max_id", mMaxId >>= minusOne)
            , ("since_id", mSinceId >>= removeEmpty)
            , ("tweet_mode", Just "extended") -- Show tweets with 280 characters
            ]

        url =
            "https://api.twitter.com/1.1/statuses/" ++ show timeline ++ ".json"

        minusOne str =
            str
            & (\v -> readMaybe v :: Maybe Integer)
            & fmap (show . flip (-) 1)

        removeEmpty str
            | null str = Nothing
            | otherwise = Just str

-------------------------------------------------------------------------------
--------------------------------- USERS ---------------------------------------
-------------------------------------------------------------------------------

searchUser :: OAuth
    -> Manager
    -> UserDetails
    -> Maybe String
    -> IO (Either String Value)
searchUser oauth manager userDetails mQuery =
    asValue <$>
    queryApi
        "GET"
        "https://api.twitter.com/1.1/users/search.json"
        queryList
        oauth
        manager
        userDetails
    where
        queryList =
            [ ("q", mQuery)
            , ("include_entities", Just "false")
            , ("count", Just "5")
            ]


postStatusUpdate :: OAuth
    -> Manager
    -> UserDetails
    -> Maybe String
    -> Maybe String
    -> IO (Either String Value)
postStatusUpdate oauth manager userDetails mStatus mInReplyToStatusId =
    asValue <$>
    queryApi
        "POST"
        "https://api.twitter.com/1.1/statuses/update.json"
        queryList
        oauth
        manager
        userDetails
    where
        queryList =
            [ ("status", mStatus)
            , ("in_reply_to_status_id", mInReplyToStatusId)
            ]

postFavorite :: OAuth
    -> Manager
    -> UserDetails
    -> Bool
    -> Maybe String
    -> IO (Either String Value)
postFavorite oauth manager userDetails isFavorite mTweetId =
    asValue <$>
    queryApi
        "POST"
        url
        [("id", mTweetId)]
        oauth
        manager
        userDetails
    where
        url =
            "https://api.twitter.com/1.1/favorites/"
            ++
                if isFavorite then
                    "create.json"
                else
                    "destroy.json"


postRetweet :: OAuth
    -> Manager
    -> UserDetails
    -> Bool
    -> Maybe String
    -> IO (Either String Value)
postRetweet oauth manager userDetails shouldRetweet mTweetId =
    asValue <$>
    queryApi
        "POST"
        url
        [("id", mTweetId)]
        oauth
        manager
        userDetails
    where
        tweetId =
            fromMaybe "" mTweetId

        endpoint =
            if shouldRetweet then
                "retweet"
            else
                "unretweet"
        url =
            traceShowId $ "https://api.twitter.com/1.1/statuses/" ++ endpoint ++ "/" ++ tweetId ++ ".json"
