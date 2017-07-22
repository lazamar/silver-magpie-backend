{-# LANGUAGE OverloadedStrings #-}

module Main where

import App (runApp)
import Data.Text as T
import Env (loadEnvironment)

main :: IO ()
main = do
    mEnv <- loadEnvironment
    case mEnv of
        Left err ->
            putStrLn $ T.unpack err
        Right env ->
            runApp env
