import Model
import Service

import Test.QuickCheck

main :: IO ()
main = do
  x <- generate generateStartingPrice
  quickCheck $ (price 0 0 x) == (startingPrice x)

generateStartingPrice = do
  startingPrice <- arbitrary
  return $ RateDef startingPrice Nothing Nothing Nothing Nothing Nothing Nothing Nothing
