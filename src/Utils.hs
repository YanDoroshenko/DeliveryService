module Utils where

import Model

import Data.Text
import Data.UUID

apply :: (UUID, Text, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double) -> PostalCodeOverrideRate
apply (id,
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

unapply :: PostalCodeOverrideRate -> (UUID, Text, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double)
unapply (PostalCodeOverrideRate
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
