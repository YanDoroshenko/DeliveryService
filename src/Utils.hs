module Utils where

import Model

import Data.Int

apply :: (Int32, Maybe Double, Double, Double, Double, Double, Double, Double, Double) -> RateDef
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

unapply :: RateDef -> (Int32, Maybe Double, Double, Double, Double, Double, Double, Double, Double)
unapply  (RateDef
  maxDistance
  startingPrice
  subtotalFactor
  lowerSubtotalThreshold
  upperSubtotalThreshold
  lowerPriceThreshold
  upperPriceThreshold
  freeSubtotalThreshold
  weightInterval) = (
  (fromIntegral maxDistance) :: Int32,
  startingPrice,
  subtotalFactor,
  lowerSubtotalThreshold,
  upperSubtotalThreshold,
  lowerPriceThreshold,
  upperPriceThreshold,
  freeSubtotalThreshold,
  weightInterval)
