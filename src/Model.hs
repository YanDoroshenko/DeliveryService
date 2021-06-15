{-# LANGUAGE TupleSections #-}
module Model where

import Data.Maybe
import Data.Text
import Data.UUID

data Rate = PostalCodeOverrideRate UUID Text RateDef | LocationOverrideRate UUID Text RateDef | BaseDistanceRate UUID (Maybe Double) (Maybe Double) RateDef | StateOverrideRate UUID Text RateDef

data RateDef = RateDef {
  startingPrice :: Maybe Double,
  subtotalFactor :: Maybe Double,
  lowerSubtotalThreshold :: Maybe Double,
  upperSubtotalThreshold :: Maybe Double,
  lowerPriceBound :: Maybe Double,
  upperPriceBound :: Maybe Double,
  freeSubtotalThreshold :: Maybe Double,
  weightInterval :: Maybe Double
                       }

price :: Double -> Double -> RateDef -> Maybe Double
price subtotal weight (RateDef startingPrice subtotalFactor lowerSubtotalThreshold upperSubtotalThreshold lowerPriceBound upperPriceBound freeSubtotalThreshold weightInterval) =
  case combine startingPrice subtotalFactor of
    Just (startingPrice, subtotalFactor) ->
      if (not $ subtotalInBounds subtotal lowerSubtotalThreshold upperSubtotalThreshold)
         then Nothing
      else if (fromMaybe False $ (<= subtotal) <$> freeSubtotalThreshold)
        then Just 0
        else Just $ applyBounds basePrice lowerPriceBound upperPriceBound where
          basePrice = (startingPrice + subtotal * subtotalFactor) * (weightFactor weight weightInterval)
    _ -> Nothing

combine :: Maybe Double -> Maybe Double -> Maybe (Double, Double)
combine (Just price) subtotalFactor = Just (price, fromMaybe 0 subtotalFactor)
combine _ subtotalFactor = (0, ) <$> subtotalFactor

overThreshold :: Double -> Double -> Bool
overThreshold x threshold = x >= threshold

weightFactor :: Double -> Maybe Double -> Double
weightFactor weight (Just weightInterval) = if (x > (fromIntegral $ round x))
                                               then x + 1
                                               else x
                                                 where
                                                   x = weight / weightInterval
weightFactor _ _ = 1

subtotalInBounds :: Double -> Maybe Double -> Maybe Double -> Bool
subtotalInBounds subtotal (Just lowerBound) (Just upperBound) = subtotal >= lowerBound && subtotal <= upperBound
subtotalInBounds subtotal (Just lowerBound) _ = subtotal >= lowerBound
subtotalInBounds subtotal _ (Just upperBound) = subtotal <= upperBound
subtotalInBounds _ _ _ = True

applyBounds :: Double -> Maybe Double -> Maybe Double -> Double
applyBounds price (Just lowerBound) (Just upperBound) = min (max price lowerBound) upperBound
applyBounds price _ (Just upperBound) = min price upperBound
applyBounds price (Just lowerBound) _ = max price lowerBound
applyBounds price _ _ = price

data Request = Request {
  distance :: Maybe Double,
  subtotal :: Maybe Double,
  weight :: Maybe Double
                       } deriving (Eq, Show)

data Response = Response (Maybe Double)
