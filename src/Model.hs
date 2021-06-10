module Model where

import Data.Text
import Data.UUID

data PostalCodeOverrideRate = PostalCodeOverrideRate {
  id :: UUID,
  postalCode :: Text,
  postalCodeRate :: RateDef
                                                     } deriving (Eq, Show)

data LocationOverrideRate = LocationPostalCodeOverrideRate {
  locationId :: Text,
  locationRate :: RateDef
                                                                     } deriving (Eq, Show)

data BaseDistanceRate = BaseDistanceRate {
  fromDistance :: Maybe Double,
  toDistance :: Maybe Double,
  distanceRate :: RateDef
                                         } deriving (Eq, Show)

data StateOverrideRate = StateOverridRate  {
  stateCode :: Text,
  stateRate :: RateDef
                                           } deriving (Eq, Show)

data RateDef = RateDef {
  startingPrice :: Maybe Double,
  subtotalFactor :: Maybe Double,
  lowerSubtotalThreshold :: Maybe Double,
  upperSubtotalThreshold :: Maybe Double,
  lowerPriceThreshold :: Maybe Double,
  upperPriceThreshold :: Maybe Double,
  freeSubtotalThreshold :: Maybe Double,
  weightInterval :: Maybe Double
                       } deriving (Eq, Show)

data Request = Request {
  distance :: Maybe Double,
  subtotal :: Maybe Double,
  weight :: Maybe Double
                       } deriving (Eq, Show)

data Response = Response { price :: Maybe Double } deriving (Eq, Show)
