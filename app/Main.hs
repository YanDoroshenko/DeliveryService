{-# LANGUAGE OverloadedStrings #-}
module Main where

import Model
import DB
import Service
import Serializers

import Control.Monad.IO.Class
import Data.Maybe
import Web.Scotty
import Network.HTTP.Types.Status

main :: IO ()
main = scotty 3000 $ do
  get "/" $
    (liftIO $ getRates) >>= json
  post "/" $ do
    x <- jsonData :: ActionM RateDef
    _ <- liftIO $ insertRate x
    json x
  post "/calculate" $ do
    x <- jsonData :: ActionM Request
    rates <- liftIO getRates
    case rates of
      rate : _ -> json $ Response $ fromMaybe 0 $ startingPrice rate
      _ -> do
        status notFound404
        text "No rates found"
