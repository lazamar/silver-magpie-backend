{-# LANGUAGE OverloadedStrings #-}

module Main where

import App (runApp)
import Env (loadEnvironment)

main :: IO ()
main = do
    mEnv <- loadEnvironment
    case mEnv of
        Left err ->
            putStrLn err
        Right env ->
            runApp env
