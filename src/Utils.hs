module Utils where

import Model

import Data.Text
import Data.Tuple
import Data.UUID

applyPostalCodeOverride :: (UUID, Text, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double) -> PostalCodeOverrideRate
applyPostalCodeOverride (id,
  postalCode,
  startingPrice,
  subtotalFactor,
  lowerSubtotalThreshold,
  upperSubtotalThreshold,
  lowerPriceThreshold,
  upperPriceThreshold,
  freeSubtotalThreshold,
  weightInterval) =
    PostalCodeOverrideRate
      id
      postalCode
      $ RateDef
        startingPrice
        subtotalFactor
        lowerSubtotalThreshold
        upperSubtotalThreshold
        lowerPriceThreshold
        upperPriceThreshold
        freeSubtotalThreshold
        weightInterval

unapplyPostalCodeOverride :: PostalCodeOverrideRate -> (UUID, Text, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double)
unapplyPostalCodeOverride (PostalCodeOverrideRate
  id
  postalCode
  (RateDef
    startingPrice
    subtotalFactor
    lowerSubtotalThreshold
    upperSubtotalThreshold
    lowerPriceThreshold
    upperPriceThreshold
    freeSubtotalThreshold
    weightInterval
  )) = (
    id,
    postalCode,
    startingPrice,
    subtotalFactor,
    lowerSubtotalThreshold,
    upperSubtotalThreshold,
    lowerPriceThreshold,
    upperPriceThreshold,
    freeSubtotalThreshold,
    weightInterval)

unapplyLocationOverride :: LocationOverrideRate -> (UUID, Text, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double)
unapplyLocationOverride (LocationOverrideRate
  id
  locationId
  (RateDef
    startingPrice
    subtotalFactor
    lowerSubtotalThreshold
    upperSubtotalThreshold
    lowerPriceThreshold
    upperPriceThreshold
    freeSubtotalThreshold
    weightInterval
  )) = (
    id,
    locationId,
    startingPrice,
    subtotalFactor,
    lowerSubtotalThreshold,
    upperSubtotalThreshold,
    lowerPriceThreshold,
    upperPriceThreshold,
    freeSubtotalThreshold,
    weightInterval)
