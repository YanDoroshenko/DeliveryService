{-# LANGUAGE OverloadedStrings #-}
module Main where

import Model
import DB
import Service
import Typeclasses
import Instances

import Control.Monad.IO.Class
import Data.Maybe
import Web.Scotty
import Network.HTTP.Types.Status

main :: IO ()
main = do
  db <- liftIO connect
  scotty 3000 $ do
    get "/" $ do
      (liftIO $ getPostalCodeRates db) >>= json
    post "/" $ do
      x <- jsonData :: ActionM PostalCodeOverrideRate
      _ <- liftIO $ insertPostalCodeRate x
      json x
    post "/calculate" $ do
      x <- jsonData :: ActionM Request
      rates <- liftIO $ getPostalCodeRates db
      case rates of
        rate : _ -> json $ Response $ price rate
        _ -> do
          status notFound404
          text "No rates found"
  liftIO $ close db
