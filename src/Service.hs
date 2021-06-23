{-# LANGUAGE TupleSections #-}
module Service where

import Model
import DB
import Storage
import Utils

import Data.Maybe
import Data.Text
import Database.CQL.IO

calculatePrice :: Request -> ClientState -> IO Response
calculatePrice (Request postalCode locationId stateCode distance subtotal weight) db =
  fmap Response $ fromMaybe (return Nothing) $ postalCodeRate `orElse` locationRate `orElse` stateRate `orElse` distanceRate where
    postalCodeRate = (\p -> (getRatesPrice subtotal weight) <$> (getPostalCodeRates (unpack p) db)) <$> postalCode
    locationRate = (\l -> (getRatesPrice subtotal weight) <$> (getLocationRates (unpack l) db)) <$> locationId
    stateRate = (\s -> (getRatesPrice subtotal weight) <$> (getStateRates (unpack s) db)) <$> stateCode
    distanceRate = (\d -> (getRatesPrice subtotal weight) <$> ((Prelude.filter (\r -> inBounds d (distanceFrom r) (distanceTo r))) <$> (getBaseDistanceRates db))) <$> distance

getRatesPrice :: Maybe Double -> Maybe Double -> [Rate] -> Maybe Double
getRatesPrice subtotal weight rates = case catMaybes $ ((price (fromMaybe 0 subtotal) (fromMaybe 0 weight)) . rate) <$> rates of
                        [] -> Nothing
                        xs -> Just $ sum xs

price :: Double -> Double -> RateDef -> Maybe Double
price subtotal weight (RateDef startingPrice subtotalFactor lowerSubtotalThreshold upperSubtotalThreshold lowerPriceBound upperPriceBound freeSubtotalThreshold weightInterval) =
  case combine startingPrice subtotalFactor of
    Just (startingPrice, subtotalFactor) ->
      if (not $ inBounds subtotal lowerSubtotalThreshold upperSubtotalThreshold)
         then Nothing
      else if (fromMaybe False $ (<= subtotal) <$> freeSubtotalThreshold)
        then Just 0
        else Just $ applyBounds basePrice lowerPriceBound upperPriceBound where
          basePrice = (startingPrice + subtotal * subtotalFactor) * (weightFactor weight weightInterval)
    _ -> Nothing

combine :: Maybe Double -> Maybe Double -> Maybe (Double, Double)
combine (Just price) subtotalFactor = Just (price, fromMaybe 0 subtotalFactor)
combine _ subtotalFactor = (0, ) <$> subtotalFactor


weightFactor :: Double -> Maybe Double -> Double
weightFactor weight (Just weightInterval) = if (x > (fromIntegral $ round x))
                                               then x + 1
                                               else x
                                                 where
                                                   x = weight / weightInterval
weightFactor _ _ = 1
