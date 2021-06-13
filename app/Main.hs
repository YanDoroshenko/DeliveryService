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
      _ <- liftIO $ insertPostalCodeRate x
      json x
    post "/calculate" $ do
      x <- jsonData :: ActionM Request
      rates <- liftIO $ getPostalCodeRates db
      _ <- debug log $ msg $ encode x
      _ <- sequence ((\r -> debug log $ msg $ encode r) <$> rates)
      case rates of
        (PostalCodeOverrideRate _ _ rate) : _ -> json $ Response $ price (fromMaybe 0 $ subtotal x) (fromMaybe 0 $ weight x) rate
        _ -> do
          status notFound404
          text "No rates found"
  liftIO $ DB.close db
