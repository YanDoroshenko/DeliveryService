{-# LANGUAGE OverloadedStrings #-}
module Service where

import DB
import Model
import Typeclasses
import Instances
import Utils

import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Data.UUID
import Database.CQL.Protocol
import Database.CQL.IO

tableQuery :: String -> [String] -> LazyText.Text
tableQuery table keyFields = LazyText.pack $ "SELECT id, " ++ (mkString keyFields) ++ "starting_price, subtotal_factor, lower_subtotal_threshold, upper_subtotal_threshold, lower_price_bound, upper_price_bound, free_subtotal_threshold, weight_interval FROM delivery." ++ table

idQuery :: (Tuple a, Tuple b) => String -> [String] -> a -> PrepQuery R a b
idQuery table keyFields parameters = prepared $ QueryString $ mconcat [(tableQuery table keyFields), LazyText.pack " WHERE id = ?;"]

--insertPostalCodeRate :: Rate -> IO ()
--insertPostalCodeRate rate = do
--  db <- connect
--  res <- cqlInsert  ("INSERT INTO delivery.postal_code_override_rates (id, postal_code, starting_price, subtotal_factor, lower_subtotal_threshold, upper_subtotal_threshold, lower_price_bound, upper_price_bound, free_subtotal_threshold, weight_interval) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?);" :: QueryString W(UUID, Text.Text, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double) ()) (unapply10 rate) db
--  _ <- close db
--  return res

insertRate :: Rate -> IO ()
insertRate s@(PostalCodeOverrideRate _ _ _) = putStrLn "Postal"
insertRate _ = putStrLn "other"
