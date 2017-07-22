{-# LANGUAGE OverloadedStrings #-}

module Main where

import App (runApp)
import Data.List (lookup)
import System.Environment (getEnvironment)
import Text.Read (readMaybe)
import Types
    ( Environment (Development, Production)
    , EnvironmentVariables (EnvironmentVariables, dbName, environment, port, twitterKey, twitterSecret)
    )

lookupEnvField :: String -> [(String, String)] -> Either String String
lookupEnvField fieldName wholeEnv =
    case lookup fieldName wholeEnv of
        Nothing ->
            Left $ fieldName ++ " not found in environment variables."
        Just val ->
            Right val

createEnviromentVariables :: [(String, String)] -> Either String EnvironmentVariables
createEnviromentVariables wholeEnv = do
    env <- lookupEnvField "ENVIRONMENT" wholeEnv >>= toEnvironment
    por <- lookupEnvField "PORT" wholeEnv >>= portToInt
    dbN <- lookupEnvField "DB_NAME" wholeEnv
    tKey <- lookupEnvField "CONSUMER_KEY" wholeEnv
    tSecret <- lookupEnvField "CONSUMER_SECRET" wholeEnv
    return (EnvironmentVariables
                { environment = env
                , port = por
                , dbName = dbN
                , twitterKey = tKey
                , twitterSecret = tSecret
                }
            )
    where
        portToInt val =
            case (readMaybe val :: Maybe Int) of
                Nothing ->
                    Left $ "Invalid port number: \"" ++ val ++ "\""
                Just p ->
                    Right p

        toEnvironment "production" = Right Production
        toEnvironment "development" = Right Development
        toEnvironment v = Left $ "Invalid development environment: \"" ++ v ++ "\""

loadEnvironment :: IO (Either String EnvironmentVariables)
loadEnvironment = do
    wholeEnv <- getEnvironment
    return $ createEnviromentVariables wholeEnv

main :: IO ()
main = do
    mEnv <- loadEnvironment
    case mEnv of
        Left err ->
            putStrLn err
        Right env ->
            runApp env
