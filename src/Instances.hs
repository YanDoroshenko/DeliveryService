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

withId x y = y <$> (x .: "id")

withRate x y = y <*> (x .: "rate")

parsePostalCodeRate :: Object -> Parser Rate
parsePostalCodeRate v = withRate v $ (withId v PostalCodeOverrideRate) <*> v .: "postalCode"

parseLocationRate :: Object -> Parser Rate
parseLocationRate v = withRate v $ (withId v LocationOverrideRate) <*> v .: "locationId"

parseBaseDistanceRate :: Object -> Parser Rate
parseBaseDistanceRate v = withRate v $ (withId v BaseDistanceRate) <*> v .:? "distanceFrom" <*> v .:? "distanceTo"

parseStateRate :: Object -> Parser Rate
parseStateRate v = withRate v $ (withId v StateOverrideRate) <*> v .: "stateCode"

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
