{-# LANGUAGE OverloadedStrings #-}
module Main where

import Model
import DB
import Utils

import Data.Monoid (mconcat)
import Data.Int
import Data.Decimal
import Control.Monad.IO.Class
import Database.CQL.Protocol

getRates db = fmap apply <$> cqlQuery ("SELECT * FROM delivery.rates;" :: QueryString R() (Int32, Decimal, Decimal, Decimal, Maybe Decimal, Decimal, Decimal, Decimal, Decimal)) () db

main :: IO ()
main =
  do
    c <- liftIO connect
    rates <- getRates c
    putStrLn $ foldr (\x y -> mconcat [y, (show x), "\n"]) "" rates
