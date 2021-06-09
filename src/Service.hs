{-# LANGUAGE OverloadedStrings #-}
module Service where

import DB
import Model
import Utils

import Data.Int
import Database.CQL.Protocol
import Database.CQL.IO

getRates :: IO [RateDef]
getRates = do
  db <- connect
  rates <- fmap apply <$> cqlQuery ("SELECT max_distance, starting_price, subtotal_factor, lower_subtotal_threshold, upper_subtotal_threshold, lower_price_threshold, upper_price_threshold, free_subtotal_threshold, weight_interval FROM delivery.rates;" :: QueryString R() (Int32, Maybe Double, Double, Double, Double, Double, Double, Double, Double)) () db
  _ <- close db
  return rates

insertRate :: RateDef -> IO ()
insertRate rate = do
  db <- connect
  res <- cqlInsert  ("INSERT INTO delivery.rates (max_distance, starting_price, subtotal_factor, lower_subtotal_threshold, upper_subtotal_threshold, lower_price_threshold, upper_price_threshold, free_subtotal_threshold, weight_interval) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?);" :: QueryString W(Int32, Maybe Double, Double, Double, Double, Double, Double, Double, Double) ()) (unapply rate) db
  _ <- close db
  return res
