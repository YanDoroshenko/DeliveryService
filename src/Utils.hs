module Utils where

import Model

import Data.Int

apply :: (Int32, Double, Double, Double, Double, Double, Double, Double, Double) -> RateDef
apply (
  maxDistance,
  startingPrice,
  subtotalFactor,
  lowerSubtotalThreshold,
  upperSubtotalThreshold,
  lowerPriceThreshold,
  upperPriceThreshold,
  freeSubtotalThreshold,
  weightInterval
      ) = RateDef
  (fromIntegral maxDistance)
  startingPrice
  subtotalFactor
  lowerSubtotalThreshold
  upperSubtotalThreshold
  lowerPriceThreshold
  upperPriceThreshold
  freeSubtotalThreshold
  weightInterval
