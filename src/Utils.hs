module Utils where

import Model

import Control.Monad
import Data.Text (Text)
import Data.UUID

mkString :: [String] -> String
mkString xs = foldr (\l r -> l ++ ", " ++ r) "" xs

orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just x) _ = Just x
orElse _ y = y

combineMaybe :: Monad m => m (Maybe a) -> m a -> m a
combineMaybe l r =
  l >>= (\x -> case x of
           Just x_ -> return x_
           _ -> r)

unapply :: Rate ->  Either (UUID, Text, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double) (UUID, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double)
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
unapply (BaseDistanceRate id
  distanceFrom
  distanceTo
  (RateDef
    startingPrice
    subtotalFactor
    lowerSubtotalThreshold
    upperSubtotalThreshold
    lowerPriceBound
    upperPriceBound
    freeSubtotalThreshold
    weightInterval
  )) = Right (id, distanceFrom, distanceTo, startingPrice,  subtotalFactor,  lowerSubtotalThreshold,  upperSubtotalThreshold,  lowerPriceBound,  upperPriceBound,  freeSubtotalThreshold,  weightInterval)

applyPostalCodeRate :: (UUID, Text, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double) -> Rate
applyPostalCodeRate (id,
  postalCode,
  startingPrice,
  subtotalFactor,
  lowerSubtotalThreshold,
  upperSubtotalThreshold,
  lowerPriceBound,
  upperPriceBound,
  freeSubtotalThreshold,
  weightInterval
                     ) = PostalCodeOverrideRate id
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
  )

applyLocationRate :: (UUID, Text, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double) -> Rate
applyLocationRate (id,
  locationId,
  startingPrice,
  subtotalFactor,
  lowerSubtotalThreshold,
  upperSubtotalThreshold,
  lowerPriceBound,
  upperPriceBound,
  freeSubtotalThreshold,
  weightInterval
                     ) = LocationOverrideRate id
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
  )

applyBaseDistanceRate :: (UUID, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double) -> Rate
applyBaseDistanceRate (id,
  distanceFrom,
  distanceTo,
  startingPrice,
  subtotalFactor,
  lowerSubtotalThreshold,
  upperSubtotalThreshold,
  lowerPriceBound,
  upperPriceBound,
  freeSubtotalThreshold,
  weightInterval
                     ) = BaseDistanceRate id
  distanceFrom
  distanceTo
  (RateDef
    startingPrice
    subtotalFactor
    lowerSubtotalThreshold
    upperSubtotalThreshold
    lowerPriceBound
    upperPriceBound
    freeSubtotalThreshold
    weightInterval
  )

applyStateRate :: (UUID, Text, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double) -> Rate
applyStateRate (id,
  stateCode,
  startingPrice,
  subtotalFactor,
  lowerSubtotalThreshold,
  upperSubtotalThreshold,
  lowerPriceBound,
  upperPriceBound,
  freeSubtotalThreshold,
  weightInterval
                     ) = StateOverrideRate id
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
  )
