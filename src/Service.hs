{-# LANGUAGE OverloadedStrings #-}
module Service where

import DB
import Model
import Utils

import Data.Text
import Data.UUID
import Database.CQL.Protocol
import Database.CQL.IO

getRates :: IO [PostalCodeOverrideRate]
getRates = do
  db <- connect
  rates <- fmap apply <$> cqlQuery ("SELECT id, postal_code, starting_price, subtotal_factor, lower_subtotal_threshold, upper_subtotal_threshold, lower_price_threshold, upper_price_threshold, free_subtotal_threshold, weight_interval FROM delivery.postal_code_override_rates;" :: QueryString R() (UUID, Text, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double)) () db
  _ <- close db
  return rates

insertRate :: PostalCodeOverrideRate -> IO ()
insertRate rate = do
  db <- connect
  res <- cqlInsert  ("INSERT INTO delivery.postal_code_override_rates (id, postal_code, starting_price, subtotal_factor, lower_subtotal_threshold, upper_subtotal_threshold, lower_price_threshold, upper_price_threshold, free_subtotal_threshold, weight_interval) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?);" :: QueryString W(UUID, Text, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double) ()) (unapply rate) db
  _ <- close db
  return res
