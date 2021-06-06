module Model where

data RateDef = RateDef {
  maxDistance :: Int,
  startingPrice :: Double,
  subtotalFactor :: Double,
  lowerSubtotalThreshold :: Double,
  upperSubtotalThreshold :: Double,
  lowerPriceThreshold :: Double,
  upperPriceThreshold :: Double,
  freeSubtotalThreshold :: Double,
  weightInterval :: Double
                 }

data Request = Request {
  distance :: Double,
  subtotal :: Double,
  weight :: Double
             }

data Response = Response { price :: Double }
