{-# LANGUAGE OverloadedStrings #-}
module Service where

import DB
import Model
import Instances
import Utils

import Control.Exception
import Data.Either
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Data.UUID
import Database.CQL.Protocol
import Database.CQL.IO

tableQuery :: String -> [String] -> LazyText.Text
tableQuery table keyFields = LazyText.pack $ "SELECT id, " ++ (mkString keyFields) ++ "starting_price, subtotal_factor, lower_subtotal_threshold, upper_subtotal_threshold, lower_price_bound, upper_price_bound, free_subtotal_threshold, weight_interval FROM delivery." ++ table

idQuery :: (Tuple a, Tuple b) => String -> [String] -> a -> PrepQuery R a b
idQuery table keyFields parameters = prepared $ QueryString $ mconcat [(tableQuery table keyFields), LazyText.pack " WHERE id = ?;"]

insertQuery :: String -> [String] -> LazyText.Text
insertQuery table keyFields = LazyText.pack $ "INSERT INTO delivery." ++ table ++ " (id, " ++ keyFieldsStr ++  "starting_price, subtotal_factor, lower_subtotal_threshold, upper_subtotal_threshold, lower_price_bound, upper_price_bound, free_subtotal_threshold, weight_interval) VALUES (?, " ++ keyValuesStr ++ "?, ?, ?, ?, ?, ?, ?, ?);" where
  keyFieldsStr = mkString keyFields
  keyValuesStr = foldr (\_ y -> "?, " ++ y) "" keyFields

insertRate :: Rate -> ClientState -> IO ()
insertRate rate@(PostalCodeOverrideRate _ _ _) db = cqlInsert (prepared $ QueryString $ insertQuery "postal_code_override_rates" ["postal_code"]) (fromLeft (throw $ TypeError "left") $ unapply rate) db
insertRate rate@(LocationOverrideRate _ _ _) db = cqlInsert (prepared $ QueryString $ insertQuery "location_override_rates" ["location_id"]) (fromLeft (throw $ TypeError "left") $ unapply rate) db
insertRate rate@(BaseDistanceRate _ _ _ _) db = cqlInsert (prepared $ QueryString $ insertQuery "base_distance_rates" ["distance_from", "distance_to"]) (fromRight (throw $ TypeError "left") $ unapply rate) db
insertRate rate@(StateOverrideRate _ _ _) db = cqlInsert (prepared $ QueryString $ insertQuery "state_override_rates" ["state_code"]) (fromLeft (throw $ TypeError "left") $ unapply rate) db

selectRates :: ClientState -> IO [Rate]
selectRates db = do
  postalCodeRates <- fmap applyPostalCodeRate <$> cqlQuery (QueryString $ tableQuery "postal_code_override_rates" ["postal_code"]) () db
  return postalCodeRates
