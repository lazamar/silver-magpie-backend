{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import App (runApp)
import App (runApp)
import Env (loadEnvironment)
import Types (environment)

main :: IO ()
main = do
    eitherEnv <- loadEnvironment
    case eitherEnv of
        Left err ->
            putStrLn err
        Right env ->
            do
                putStrLn
                    $ (++) "Running on environment: "
                    $ show
                    $ environment env
                runApp env
