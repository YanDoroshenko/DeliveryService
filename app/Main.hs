{-# LANGUAGE OverloadedStrings #-}
module Main where

import Model
import DB
import Service
import Typeclasses
import Instances

import Control.Monad.IO.Class
import Data.Maybe
import System.Logger
import Web.Scotty
import Network.HTTP.Types.Status
import Data.Aeson (encode)

main :: IO ()
main = do
  db <- liftIO connect
  log <- new defSettings
  scotty 3000 $ do
    get "/" $ do
      --(liftIO $ getPostalCodeRates db) >>= json
      text "lol"
    post "/" $ do
      x <- jsonData :: ActionM Rate
      _ <- debug log $ msg $ mconcat ["Create rate - request: ", encode x]
      _ <- liftIO $ insertRate x
      json x
    post "/calculate" $ do
      x <- jsonData :: ActionM Request
      _ <- debug log $ msg $ mconcat ["Calculate price - request: ", encode x]
      _ <- warn log $ msg ("No rates found" :: String)
      status notFound404
      text "No rates found"
  liftIO $ DB.close db
