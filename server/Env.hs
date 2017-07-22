{-
    Loads environemnt variables
-}
{-# LANGUAGE OverloadedStrings #-}

module Env (loadEnvironment) where

import Data.List (lookup)
import Data.Text (Text)
import Data.Text as T
import System.Environment (getEnvironment)
import Text.Read (readMaybe)
import Types
    ( Environment (Development, Production)
    , EnvironmentVariables (EnvironmentVariables, dbName, dbUrl, environment, port, twitterKey, twitterSecret)
    )

lookupEnvField :: String -> [(String, String)] -> Either Text Text
lookupEnvField fieldName wholeEnv =
    case lookup fieldName wholeEnv of
        Nothing ->
            Left $ T.pack $ fieldName ++ " not found in environment variables."
        Just val ->
            Right $ T.pack val

createEnviromentVariables :: [(String, String)] -> Either Text EnvironmentVariables
createEnviromentVariables wholeEnv = do
    env <- lookupEnvField "ENVIRONMENT" wholeEnv >>= toEnvironment
    por <- lookupEnvField "PORT" wholeEnv >>= portToInt
    dbN <- lookupEnvField "DB_NAME" wholeEnv
    url <- lookupEnvField "DB_URL" wholeEnv
    tKey <- lookupEnvField "CONSUMER_KEY" wholeEnv
    tSecret <- lookupEnvField "CONSUMER_SECRET" wholeEnv
    return (EnvironmentVariables
                { environment = env
                , port = por
                , dbName = dbN
                , dbUrl = url
                , twitterKey = tKey
                , twitterSecret = tSecret
                }
            )

portToInt :: Text -> Either Text Int
portToInt val =
    case (readMaybe (T.unpack val) :: Maybe Int) of
        Nothing ->
            Left $ T.pack $ "Invalid port number: \"" ++ (T.unpack val) ++ "\""
        Just p ->
            Right p

toEnvironment :: Text -> Either Text Environment
toEnvironment "production" = Right Production
toEnvironment "development" = Right Development
toEnvironment v = Left $ T.pack $ "Invalid environment value: \"" ++ (T.unpack v) ++ "\""

loadEnvironment :: IO (Either Text EnvironmentVariables)
loadEnvironment = do
    wholeEnv <- getEnvironment
    return $ createEnviromentVariables wholeEnv
