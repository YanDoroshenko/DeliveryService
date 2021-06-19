{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
module Instances where

import Model
import Utils (or)

import GHC.Generics
import Control.Applicative (empty)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Maybe

instance FromJSON Rate where
  parseJSON (Object v) = do
    p <- v .:? "postalCode" :: Parser (Maybe String)
    l <- v .:? "locationId" :: Parser (Maybe String)
    s <- v .:? "stateCode" :: Parser (Maybe String)
    fromMaybe (parseBaseDistanceRate v) $ ((\_ -> parsePostalCodeRate v) <$> p) `Utils.or` ((\_ -> parseLocationRate v) <$> l) `Utils.or` ((\_ -> parseStateRate v) <$> s)
  parseJSON _ = empty


parsePostalCodeRate :: Object -> Parser Rate
parsePostalCodeRate = \v -> PostalCodeOverrideRate
      <$> v .: "id"
      <*> v .: "postalCode"
      <*> v .: "rate"

parseLocationRate :: Object -> Parser Rate
parseLocationRate  = \v -> LocationOverrideRate
      <$> v .: "id"
      <*> v .: "locationId"
      <*> v .: "rate"

parseBaseDistanceRate :: Object -> Parser Rate
parseBaseDistanceRate = \v -> BaseDistanceRate
  <$> v .: "id"
  <*> v .:? "distanceFrom"
  <*> v .:? "distanceTo"
  <*> v .: "rate"

parseStateRate :: Object -> Parser Rate
parseStateRate = \v -> StateOverrideRate
  <$> v .: "id"
  <*> v .: "stateCode"
  <*> v .: "rate"

instance ToJSON Rate

instance FromJSON Request
instance ToJSON Request

instance ToJSON Response

instance FromJSON RateDef
instance ToJSON RateDef

deriving instance Generic Rate

deriving instance Generic RateDef

deriving instance Generic Request

deriving instance Generic Response
