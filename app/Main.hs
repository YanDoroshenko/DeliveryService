{-# LANGUAGE OverloadedStrings #-}
module Main where

import Model
import DB
import Service
import Storage
import Instances

import Control.Monad.IO.Class
import Data.Maybe
import System.Logger
import Web.Scotty
import Network.HTTP.Types.Status
import Data.Aeson (decode, encode, Object)

main = do
  db <- liftIO connect
  log <- new defSettings
  scotty 3000 $ do
    get "/" $ (liftIO $ selectRates db) >>= json
    post "/" $ do
      rates <- jsonData :: ActionM [Rate]
      _ <- debug log $ msg $ mconcat ["Create rates - request: ", encode rates]
      _ <- liftIO $ sequence $ (\x -> insertRate x db) <$> rates
      status created201
    post "/calculate" $ do
      req <- jsonData :: ActionM Request
      _ <- debug log $ msg $ mconcat ["Calculate price - request: ", encode req]
      (liftIO $ calculatePrice req db) >>= json
  liftIO $ DB.close db
