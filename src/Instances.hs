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

instance FromJSON StateOverrideRate
instance ToJSON StateOverrideRate

instance FromJSON Request

instance ToJSON Response

instance FromJSON RateDef
instance ToJSON RateDef

deriving instance Generic PostalCodeOverrideRate

deriving instance Generic LocationOverrideRate

deriving instance Generic StateOverrideRate

deriving instance Generic RateDef

deriving instance Generic Request

deriving instance Generic Response

instance RateTuple10 PostalCodeOverrideRate where
  apply10 (id, postalCode, startingPrice, subtotalFactor, lowerSubtotalThreshold, upperSubtotalThreshold, lowerPriceBound, upperPriceBound, freeSubtotalThreshold, weightInterval) = PostalCodeOverrideRate id postalCode $ RateDef startingPrice subtotalFactor lowerSubtotalThreshold upperSubtotalThreshold lowerPriceBound upperPriceBound freeSubtotalThreshold weightInterval
  unapply10 (PostalCodeOverrideRate id postalCode (RateDef startingPrice subtotalFactor lowerSubtotalThreshold upperSubtotalThreshold lowerPriceBound upperPriceBound freeSubtotalThreshold weightInterval)) = (id, postalCode, startingPrice, subtotalFactor, lowerSubtotalThreshold, upperSubtotalThreshold, lowerPriceBound, upperPriceBound, freeSubtotalThreshold, weightInterval)

instance RateTuple10 LocationOverrideRate where
  apply10 (id, locationId, startingPrice, subtotalFactor, lowerSubtotalThreshold, upperSubtotalThreshold, lowerPriceBound, upperPriceBound, freeSubtotalThreshold, weightInterval) = LocationOverrideRate id locationId $ RateDef startingPrice subtotalFactor lowerSubtotalThreshold upperSubtotalThreshold lowerPriceBound upperPriceBound freeSubtotalThreshold weightInterval
  unapply10 (LocationOverrideRate id locationId (RateDef startingPrice subtotalFactor lowerSubtotalThreshold upperSubtotalThreshold lowerPriceBound upperPriceBound freeSubtotalThreshold weightInterval)) = (id, locationId, startingPrice, subtotalFactor, lowerSubtotalThreshold, upperSubtotalThreshold, lowerPriceBound, upperPriceBound, freeSubtotalThreshold, weightInterval)

instance RateTuple10 StateOverrideRate where
  apply10 (id, stateCode, startingPrice, subtotalFactor, lowerSubtotalThreshold, upperSubtotalThreshold, lowerPriceBound, upperPriceBound, freeSubtotalThreshold, weightInterval) = StateOverrideRate id stateCode $ RateDef startingPrice subtotalFactor lowerSubtotalThreshold upperSubtotalThreshold lowerPriceBound upperPriceBound freeSubtotalThreshold weightInterval
  unapply10 (StateOverrideRate id stateCode (RateDef startingPrice subtotalFactor lowerSubtotalThreshold upperSubtotalThreshold lowerPriceBound upperPriceBound freeSubtotalThreshold weightInterval)) = (id, stateCode, startingPrice, subtotalFactor, lowerSubtotalThreshold, upperSubtotalThreshold, lowerPriceBound, upperPriceBound, freeSubtotalThreshold, weightInterval)

