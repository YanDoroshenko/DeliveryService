{-# LANGUAGE OverloadedStrings #-}
module Main where

import Model
import DB
import Service

import Data.Monoid (mconcat)
import Control.Monad.IO.Class
import System.Random

main :: IO ()
main =
  do
    c <- liftIO connect
    rates <- getRates c
    id <- randomRIO (1, 10)
    _ <- insertRate (RateDef id Nothing 2 3 4 5 6 7 8) c
    putStrLn $ foldr (\x y -> mconcat [y, (show x), "\n"]) "" rates
    close c
