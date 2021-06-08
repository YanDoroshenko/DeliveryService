{-# LANGUAGE DeriveGeneric #-}
module Model where

import GHC.Generics

data RateDef = RateDef {
  maxDistance :: Int,
  startingPrice :: Maybe Double,
  subtotalFactor :: Double,
  lowerSubtotalThreshold :: Double,
  upperSubtotalThreshold :: Double,
  lowerPriceThreshold :: Double,
  upperPriceThreshold :: Double,
  freeSubtotalThreshold :: Double,
  weightInterval :: Double
                 } deriving (Eq, Show, Generic)

data Request = Request {
  distance :: Double,
  subtotal :: Double,
  weight :: Double
             }

data Response = Response { price :: Double }
