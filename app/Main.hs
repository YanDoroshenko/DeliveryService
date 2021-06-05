{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib

import Web.Scotty
import Data.Text.Lazy.Encoding (decodeUtf8)

main :: IO ()
main = scotty 3000 $
    get "/:str" $ do
        str <- param "str"
        json (str :: String)
