{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
module Instances where

import Model
import Typeclasses

import GHC.Generics
import Data.Aeson

instance FromJSON PostalCodeOverrideRate
instance ToJSON PostalCodeOverrideRate

instance FromJSON LocationOverrideRate
instance ToJSON LocationOverrideRate

instance FromJSON Request

instance ToJSON Response

instance FromJSON RateDef
instance ToJSON RateDef

deriving instance Generic PostalCodeOverrideRate

deriving instance Generic LocationOverrideRate

deriving instance Generic RateDef

deriving instance Generic Request

deriving instance Generic Response

instance Price PostalCodeOverrideRate where
  price (PostalCodeOverrideRate _ _ rate) = startingPrice rate

instance Price LocationOverrideRate where
  price (LocationOverrideRate _ _ rate) = startingPrice rate

instance RateTuple10 PostalCodeOverrideRate where
  apply10 (id, postalCode, startingPrice, subtotalFactor, lowerSubtotalThreshold, upperSubtotalThreshold, lowerPriceThreshold, upperPriceThreshold, freeSubtotalThreshold, weightInterval) = PostalCodeOverrideRate id postalCode $ RateDef startingPrice subtotalFactor lowerSubtotalThreshold upperSubtotalThreshold lowerPriceThreshold upperPriceThreshold freeSubtotalThreshold weightInterval
  unapply10 (PostalCodeOverrideRate id postalCode (RateDef startingPrice subtotalFactor lowerSubtotalThreshold upperSubtotalThreshold lowerPriceThreshold upperPriceThreshold freeSubtotalThreshold weightInterval)) = (id, postalCode, startingPrice, subtotalFactor, lowerSubtotalThreshold, upperSubtotalThreshold, lowerPriceThreshold, upperPriceThreshold, freeSubtotalThreshold, weightInterval)

instance RateTuple10 LocationOverrideRate where
  apply10 (id, locationId, startingPrice, subtotalFactor, lowerSubtotalThreshold, upperSubtotalThreshold, lowerPriceThreshold, upperPriceThreshold, freeSubtotalThreshold, weightInterval) = LocationOverrideRate id locationId $ RateDef startingPrice subtotalFactor lowerSubtotalThreshold upperSubtotalThreshold lowerPriceThreshold upperPriceThreshold freeSubtotalThreshold weightInterval
  unapply10 (LocationOverrideRate id locationId (RateDef startingPrice subtotalFactor lowerSubtotalThreshold upperSubtotalThreshold lowerPriceThreshold upperPriceThreshold freeSubtotalThreshold weightInterval)) = (id, locationId, startingPrice, subtotalFactor, lowerSubtotalThreshold, upperSubtotalThreshold, lowerPriceThreshold, upperPriceThreshold, freeSubtotalThreshold, weightInterval)

