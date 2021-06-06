module Utils where

import Model

import Data.Int
import Data.Decimal

apply :: (Int32, Decimal, Decimal, Decimal, Maybe Decimal, Decimal, Decimal, Decimal, Decimal) -> RateDef
apply (
  maxDistance,
  subtotalFactor,
  lowerSubtotalThreshold,
  upperSubtotalThreshold,
  startingPrice,
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
