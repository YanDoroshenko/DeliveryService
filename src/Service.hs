{-# LANGUAGE OverloadedStrings #-}
module Service where

import DB
import Model
import Typeclasses
import Instances

import Data.Monoid (mconcat)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Data.UUID
import Database.CQL.Protocol
import Database.CQL.IO

buildQuery :: Tuple a => String -> String -> QueryString R () a
buildQuery table keyField = QueryString $ LazyText.pack $ "SELECT id " ++ keyField ++ ", starting_price, subtotal_factor, lower_subtotal_threshold, upper_subtotal_threshold, lower_price_threshold, upper_price_threshold, free_subtotal_threshold, weight_interval FROM delivery." ++ table ++ ";"

getRates10 :: RateTuple10 a => ClientState -> String-> String -> IO [a]
getRates10 db table keyField = fmap apply10 <$> cqlQuery (buildQuery table keyField) () db

getPostalCodeRates :: ClientState -> IO [PostalCodeOverrideRate]
getPostalCodeRates db = getRates10 db "postal_code_override_rates" "postal_code"

insertPostalCodeRate :: PostalCodeOverrideRate -> IO ()
insertPostalCodeRate rate = do
  db <- connect
  res <- cqlInsert  ("INSERT INTO delivery.postal_code_override_rates (id, postal_code, starting_price, subtotal_factor, lower_subtotal_threshold, upper_subtotal_threshold, lower_price_threshold, upper_price_threshold, free_subtotal_threshold, weight_interval) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?);" :: QueryString W(UUID, Text.Text, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double) ()) (unapply10 rate) db
  _ <- close db
  return res
