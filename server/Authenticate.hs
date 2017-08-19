module Authenticate (authenticate) where

import MongoTypes.UserDetails (UserDetails (UserDetails))
import Types (DBActionRunner)

authenticate :: DBActionRunner -> String -> IO (Maybe UserDetails)
authenticate runDbAction sessionId =
    runDbAction $ getUserDetailsFromSessionId sessionId


getUserDetailsFromSessionId :: MonadIO m => String -> Action m (Maybe UserDetails)
getUserDetailsFromSessionId sessionId =
    do
        mToken <- (accessRequestToken =<<) <$> getRequestTokenBySessionId sessionId
        case mToken of
            Nothing ->
                return Nothing
            Just token ->
                getUserDetailsByRequestToken token
    where
        accessRequestToken doc =
            Bson.lookup "access_request_token" doc ::Maybe String

getRequestTokenBySessionId :: MonadIO m => String -> Action m (Maybe AppAuth)
getRequestTokenBySessionId sessionId =
    let
        selector = [ "app_session_id" =: sessionId ]
        collection = "app-authorisation"
        selection = Mongo.select selector collection
    in
        findOne selection
