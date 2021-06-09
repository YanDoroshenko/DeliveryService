module Model where

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
                 } deriving (Eq, Show)

data Request = Request {
  distance :: Double,
  subtotal :: Double,
  weight :: Double
             } deriving (Eq, Show)

data Response = Response { price :: Double } deriving (Eq, Show)
