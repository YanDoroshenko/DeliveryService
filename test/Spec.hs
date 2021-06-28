import Model
import Service

import Test.QuickCheck

main :: IO ()
main = do
  _ <- checkStartingPrice
  checkSubtotalFactor

checkStartingPrice = do
  rate <- generate generateStartingPrice
  subtotal <- generate arbitrary
  weight <- generate arbitrary
  quickCheck $ (price subtotal weight rate) == (startingPrice rate)

checkSubtotalFactor = do
  rate <- generate generateSubtotalFactor
  subtotal <- generate arbitrary
  weight <- generate arbitrary
  quickCheck $ (price subtotal weight rate) == ((subtotal *) <$> (subtotalFactor rate))

generateStartingPrice = do
  startingPrice <- arbitrary
  return $ RateDef startingPrice Nothing Nothing Nothing Nothing Nothing Nothing Nothing

generateSubtotalFactor = do
  subtotalFactor <- arbitrary
  return $ RateDef Nothing subtotalFactor Nothing Nothing Nothing Nothing Nothing Nothing
