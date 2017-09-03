{-# LANGUAGE DeriveGeneric #-}

module Twitter.Timeline (Timeline, WhichTimeline(..), fetchTimeline, tweets) where

import Data.Aeson (Value)
import Data.Aeson.Types (ToJSON)
import GHC.Generics (Generic)
import MongoTypes.UserDetails (UserDetails)
import Network.HTTP.Client (Manager)
import qualified Twitter.Query
import qualified Web.Authenticate.OAuth as OAuth

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
    -> OAuth.OAuth
    -> Manager
    -> UserDetails
    -> Maybe String
    -> Maybe String
    -> IO (Either String Timeline)
fetchTimeline timeline oauth manager userDetails mSinceId mMaxId =
    fmap Timeline <$>
        Twitter.Query.get url queryList oauth manager userDetails

    where
        queryList =
            [ ("max_id", mMaxId >>= removeEmpty)
            , ("since_id", mSinceId >>= removeEmpty)
            ]

        url =
            "https://api.twitter.com/1.1/statuses/" ++ show timeline ++ ".json"

        removeEmpty v =
            if null v then
                Nothing
            else
                Just v
