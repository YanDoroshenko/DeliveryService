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
      (liftIO $ getPostalCodeRates db) >>= json
    post "/" $ do
      x <- jsonData :: ActionM PostalCodeOverrideRate
      _ <- debug log $ msg $ mconcat ["Create postal code override rate - request: ", encode x]
      _ <- liftIO $ insertPostalCodeRate x
      json x
    post "/calculate" $ do
      x <- jsonData :: ActionM Request
      _ <- debug log $ msg $ mconcat ["Calculate price - request: ", encode x]
      rates <- liftIO $ getPostalCodeRates db
      case rates of
        (PostalCodeOverrideRate _ _ rate) : _ ->  do
          let price_ = price (fromMaybe 0 $ subtotal x) (fromMaybe 0 $ weight x) rate
          let response = Response $ price_
          _ <- debug log $ msg $ mconcat ["Calculate price - response: ", encode response]
          json response
        _ -> do
          _ <- warn log $ msg ("No rates found" :: String)
          status notFound404
          text "No rates found"
  liftIO $ DB.close db
