{-# LANGUAGE OverloadedStrings #-}
module Main where

import Model
import DB
import Service
import Serializers

import Control.Monad.IO.Class
import Web.Scotty

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

