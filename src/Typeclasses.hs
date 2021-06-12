module Typeclasses where

import Model

import Data.Text
import Data.UUID

class RateTuple10 a where
  apply10 :: (UUID, Text, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double) -> a
  unapply10 :: a -> (UUID, Text, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double)

class RateTuple11 a where
  apply11 :: (UUID, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double) -> a
  unapply11 :: a -> (UUID, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double, Maybe Double)

