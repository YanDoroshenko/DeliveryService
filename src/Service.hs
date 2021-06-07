{-# LANGUAGE OverloadedStrings #-}
module Service where

import DB
import Model
import Utils

import Data.Int
import Data.Decimal
import Database.CQL.Protocol
import Database.CQL.IO

getRates :: ClientState -> IO [RateDef]
getRates db = fmap apply <$> cqlQuery ("SELECT max_distance, starting_price, subtotal_factor, lower_subtotal_threshold, upper_subtotal_threshold, lower_price_threshold, upper_price_threshold, free_subtotal_threshold, weigth_interval FROM delivery.rates;" :: QueryString R() (Int32, Maybe Decimal, Decimal, Decimal, Decimal, Decimal, Decimal, Decimal, Decimal)) () db

insertRate :: RateDef -> ClientState -> IO ()
insertRate rate db = cqlInsert  ("INSERT INTO delivery.rates (max_distance, starting_price, subtotal_factor, lower_subtotal_threshold, upper_subtotal_threshold, lower_price_threshold, upper_price_threshold, free_subtotal_threshold, weigth_interval) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?);" :: QueryString W(Int32, Maybe Decimal, Decimal, Decimal, Decimal, Decimal, Decimal, Decimal, Decimal) ()) (unapply rate) db
