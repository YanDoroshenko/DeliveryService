{-# LANGUAGE DuplicateRecordFields #-}
module Model (Rate(..), RateDef(..), Request(..), Response(..)) where

import Data.Maybe
import Data.Text
import Data.UUID

data Rate = PostalCodeOverrideRate { id :: UUID, postalCode :: Text, rate :: RateDef } | LocationOverrideRate { id :: UUID, locationId :: Text, rate :: RateDef } | BaseDistanceRate { id :: UUID, distanceFrom :: (Maybe Double), distanceTo :: (Maybe Double), rate :: RateDef } | StateOverrideRate { id :: UUID, stateCode :: Text, rate :: RateDef }

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

data Request = Request (Maybe Text) (Maybe Text) (Maybe Text) (Maybe Double) (Maybe Double) (Maybe Double)

data Response = Response (Maybe Double)
