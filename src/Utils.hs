module Utils where

import Model

import Data.Text (Text)
import Data.UUID

mkString :: [String] -> String
mkString xs = foldr (\l r -> l ++ ", " ++ r) "" xs

unapply :: Rate ->  Either (UUID, Text, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double) (UUID, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double)
unapply (PostalCodeOverrideRate id
  postalCode
  (RateDef
    startingPrice
    subtotalFactor
    lowerSubtotalThreshold
    upperSubtotalThreshold
    lowerPriceBound
    upperPriceBound
    freeSubtotalThreshold
    weightInterval
  )) = Left (id, postalCode, startingPrice,  subtotalFactor,  lowerSubtotalThreshold,  upperSubtotalThreshold,  lowerPriceBound,  upperPriceBound,  freeSubtotalThreshold,  weightInterval)
unapply (LocationOverrideRate id
  locationId
  (RateDef
    startingPrice
    subtotalFactor
    lowerSubtotalThreshold
    upperSubtotalThreshold
    lowerPriceBound
    upperPriceBound
    freeSubtotalThreshold
    weightInterval
  )) = Left (id, locationId, startingPrice,  subtotalFactor,  lowerSubtotalThreshold,  upperSubtotalThreshold,  lowerPriceBound,  upperPriceBound,  freeSubtotalThreshold,  weightInterval)
unapply (StateOverrideRate id
  stateCode
  (RateDef
    startingPrice
    subtotalFactor
    lowerSubtotalThreshold
    upperSubtotalThreshold
    lowerPriceBound
    upperPriceBound
    freeSubtotalThreshold
    weightInterval
  )) = Left (id, stateCode, startingPrice,  subtotalFactor,  lowerSubtotalThreshold,  upperSubtotalThreshold,  lowerPriceBound,  upperPriceBound,  freeSubtotalThreshold,  weightInterval)
