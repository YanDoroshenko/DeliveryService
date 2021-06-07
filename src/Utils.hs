module Utils where

import Model

import Data.Int
import Data.Decimal

apply :: (Int32, Maybe Decimal, Decimal, Decimal, Decimal, Decimal, Decimal, Decimal, Decimal) -> RateDef
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

unapply :: RateDef -> (Int32, Maybe Decimal, Decimal, Decimal, Decimal, Decimal, Decimal, Decimal, Decimal)
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
