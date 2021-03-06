{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
module Instances where

import Model
import Utils (combineMaybe)

import GHC.Generics
import Control.Applicative (empty)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)

instance FromJSON Rate where
  parseJSON (Object v) =
    (parse "postalCode" parsePostalCodeRate v) `combineMaybe` (
    (parse "locationId" parseLocationRate v) `combineMaybe` (
    (parse "stateCode" parseStateRate v) `combineMaybe`
    (return $ parseBaseDistanceRate v))) >>= Prelude.id
  parseJSON _ = empty

instance ToJSON Rate

instance FromJSON Request where
  parseJSON (Object v) = Request <$>
    v .:? "postalCode" <*>
    v .:? "locationId" <*>
    v .:? "stateCode" <*>
    v .:? "distance" <*>
    v .:? "subtotal" <*>
    v .:? "weight"
  parseJSON _ = empty

instance ToJSON Request

instance ToJSON Response

instance FromJSON RateDef
instance ToJSON RateDef

deriving instance Generic Rate

deriving instance Generic RateDef

deriving instance Generic Request

deriving instance Generic Response

parse :: Text -> (Object -> Parser Rate) -> Object -> Parser (Maybe (Parser Rate))
parse key f v = ((\p -> (\_ -> f v) <$> p) <$> ((v .:? key) :: Parser (Maybe String)))

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

