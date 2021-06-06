module Model where

import Data.Decimal

data RateDef = RateDef {
  maxDistance :: Int,
  startingPrice :: Maybe Decimal,
  subtotalFactor :: Decimal,
  lowerSubtotalThreshold :: Decimal,
  upperSubtotalThreshold :: Decimal,
  lowerPriceThreshold :: Decimal,
  upperPriceThreshold :: Decimal,
  freeSubtotalThreshold :: Decimal,
  weightInterval :: Decimal
                 } deriving (Eq, Show)

data Request = Request {
  distance :: Decimal,
  subtotal :: Decimal,
  weight :: Decimal
             }

data Response = Response { price :: Decimal }
