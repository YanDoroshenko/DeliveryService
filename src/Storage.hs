{-# LANGUAGE OverloadedStrings #-}
module Storage (insertRate, selectRates, getPostalCodeRate) where

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

keyQuery :: (Tuple a, Tuple b) => String -> String -> PrepQuery R a b
keyQuery table keyField = prepared $ QueryString $ mconcat [(tableQuery table [keyField]), LazyText.pack " WHERE ", LazyText.pack keyField, LazyText.pack " = ?;"]

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
  locationRates <- fmap applyLocationRate <$> cqlQuery (QueryString $ tableQuery "location_override_rates" ["location_id"]) () db
  baseDistanceRates <- fmap applyBaseDistanceRate <$> cqlQuery (QueryString $ tableQuery "base_distance_rates" ["distance_from", "distance_to"]) () db
  stateOverrideRates <- fmap applyStateRate <$> cqlQuery (QueryString $ tableQuery "state_override_rates" ["state_code"]) () db
  return $ postalCodeRates ++ locationRates ++ baseDistanceRates ++ stateOverrideRates

getPostalCodeRates :: String -> ClientState -> IO [Rate]
getPostalCodeRates postalCode db = fmap applyPostalCodeRate <$> cqlQuery (keyQuery "postal_code_rates_by_key" "postal_code") (Identity $ Text.pack postalCode) db

getLocationRates :: String -> ClientState -> IO [Rate]
getLocationRates locationId db = fmap applyLocationRate <$> cqlQuery (keyQuery "location_rates_by_key" "location") (Identity $ Text.pack locationId) db

getStateRates :: String -> ClientState -> IO [Rate]
getStateRates stateCode db = fmap applyStateRate <$> cqlQuery (keyQuery "state_rates_by_key" "stateCode") (Identity $ Text.pack stateCode) db

getBaseDistanceRates :: ClientState -> IO [Rate]
getBaseDistanceRates db = fmap applyBaseDistanceRate <$> cqlQuery (QueryString $ tableQuery "base_distance_rates" ["distance_from", "distance_to"]) () db
