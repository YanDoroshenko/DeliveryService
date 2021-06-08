{-# LANGUAGE OverloadedStrings #-}
module Main where

import Model
import DB
import Service

import Data.Monoid (mconcat)
import Control.Monad.IO.Class
import Web.Scotty
import Data.Aeson (FromJSON, ToJSON)

instance FromJSON RateDef
instance ToJSON RateDef

main :: IO ()
main = scotty 3000 $ do
  get "/" $ do
    c <- liftIO connect
    rates <- liftIO $ getRates c
    _ <- liftIO $ close c
    json rates
  post "/" $ do
    x <- jsonData :: ActionM RateDef
    c <- liftIO connect
    _ <- liftIO $ insertRate x c
    _ <- liftIO $ close c
    json x

