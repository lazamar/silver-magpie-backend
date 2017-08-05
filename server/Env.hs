{-
    Loads environemnt variables
-}
{-# LANGUAGE OverloadedStrings #-}

module Env (loadEnvironment) where

import Data.Either.Combinators (mapLeft)
import qualified Data.Yaml as Yaml
import Types (EnvironmentVariables)

loadEnvironment :: IO (Either String EnvironmentVariables)
loadEnvironment = do
    envWithParseException <-
        Yaml.decodeFileEither "env.yaml"
        :: IO (Either Yaml.ParseException EnvironmentVariables)
    -- Transform ParseException into Text
    return $ mapLeft show envWithParseException
