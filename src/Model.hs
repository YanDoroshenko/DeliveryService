module Model where

import Data.Text
import Data.UUID

data PostalCodeOverrideRate = PostalCodeOverrideRate UUID Text RateDef

data LocationOverrideRate = LocationOverrideRate UUID Text RateDef

data BaseDistanceRate = BaseDistanceRate UUID (Maybe Double) (Maybe Double) RateDef

data StateOverrideRate = StateOverridRate UUID Text RateDef

data RateDef = RateDef {
  startingPrice :: Maybe Double,
  subtotalFactor :: Maybe Double,
  lowerSubtotalThreshold :: Maybe Double,
  upperSubtotalThreshold :: Maybe Double,
  lowerPriceThreshold :: Maybe Double,
  upperPriceThreshold :: Maybe Double,
  freeSubtotalThreshold :: Maybe Double,
  weightInterval :: Maybe Double
                       }

data Request = Request {
  distance :: Maybe Double,
  subtotal :: Maybe Double,
  weight :: Maybe Double
                       } deriving (Eq, Show)

data Response = Response (Maybe Double)
