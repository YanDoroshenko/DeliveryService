import Model
import Service

import Data.Maybe
import Test.QuickCheck

main :: IO ()
main = do
  _ <- checkStartingPrice
  _ <- checkSubtotalFactor
  _ <- checkWeightFactor
  _ <- checkLowerSubtotalThreshold1
  checkLowerSubtotalThreshold2

checkStartingPrice = do
  rate <- generate generateStartingPrice
  subtotal <- generate arbitrary
  weight <- generate arbitrary
  verboseCheck $ (price subtotal weight rate) == (startingPrice rate)

checkSubtotalFactor = do
  rate <- generate generateSubtotalFactor
  subtotal <- generate arbitrary
  weight <- generate arbitrary
  quickCheck $ (price subtotal weight rate) == ((subtotal *) <$> (subtotalFactor rate))

checkWeightFactor = do
  rate <- generate generateWeightInterval
  subtotal <- generate arbitrary
  weight <- generate arbitrary
  quickCheck $ (price subtotal weight rate) == (((\w -> (*) (fromIntegral $ ceiling $ weight / w)) <$> (weightInterval rate)) <*> (startingPrice rate))

checkLowerSubtotalThreshold1 = do
  rate <- generate generateLowerSubtotalThreshold
  subtotal <- generate $ suchThat arbitrary (\x -> fromMaybe True $ (x >= ) <$> (lowerSubtotalThreshold rate))
  weight <- generate arbitrary
  quickCheck $ (price subtotal weight rate) == (startingPrice rate)

checkLowerSubtotalThreshold2 = do
  rate <- generate generateLowerSubtotalThreshold
  subtotal <- generate $ suchThat arbitrary (\x -> fromMaybe False $ (x < ) <$> (lowerSubtotalThreshold rate))
  weight <- generate arbitrary
  quickCheck $ (price subtotal weight rate) == Nothing

generateStartingPrice = do
  startingPrice <- arbitrary
  return $ RateDef startingPrice Nothing Nothing Nothing Nothing Nothing Nothing Nothing

generateSubtotalFactor = do
  subtotalFactor <- arbitrary
  return $ RateDef Nothing subtotalFactor Nothing Nothing Nothing Nothing Nothing Nothing

generateWeightInterval = do
  startingPrice <- arbitrary
  weightInterval <- arbitrary
  return $ RateDef startingPrice Nothing Nothing Nothing Nothing Nothing Nothing weightInterval

generateLowerSubtotalThreshold = do
  startingPrice <- arbitrary
  threshold <- arbitrary
  return $ RateDef startingPrice Nothing threshold Nothing Nothing Nothing Nothing Nothing
